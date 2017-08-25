{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Server
    ( server
    , Config (..)
    , HandlerT
    ) where

import           Control.Monad (unless)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Maybe (isNothing)
import           Data.Monoid ((<>), Sum(..))
import           Data.List (scanl', last, uncons)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Jose.Jwk
import           Prelude hiding (id)
import           Servant ((:<|>) ((:<|>)), ServerT, ServantErr, Handler, NoContent(..), err400, err401, err403, err409, err404, errBody)

import           Api.Auth (AccessScope(..), mkAccessToken, scopeSubjectId)
import           Api.Types hiding (AccessToken)
import           DB (DB)
import qualified DB
import           Password

data Config db = Config
    { database :: db
    , tokenKey :: Jwk
    , sampleStories :: [Story]
    }

type HandlerT db = LoggingT (ReaderT (Config db) Handler)

type ApiServer a db = ServerT a (HandlerT db)

runDB :: MonadReader (Config b) m => (b -> m b1) -> m b1
runDB f = ask >>= f . database

server :: DB db => ApiServer Api db
server = storyServer :<|> dictServer :<|> schoolsServer :<|> schoolServer :<|> trailsServer :<|> loginServer :<|> accountServer

newUUID :: HandlerT db Text
newUUID = liftIO (toText <$> nextRandom)

loginServer :: DB db => ApiServer LoginApi db
loginServer authReq = do
    logInfoN $ "Login request from: " <> uName
    user <- runDB $ DB.getAccountByUsername uName
    case user of
        Nothing -> logInfoN ("User not found: " <> uName) >> throwError err401
        Just a -> do
            unless (validatePassword (encodeUtf8 (password (authReq :: LoginRequest))) (encodeUtf8 (password (a :: Account))))
                (throwError err401)
            -- new account which hasn't been enabled yet
            unless (active a) (throwError err403)
            (accessToken, nm) <- createToken a

            return $ Login (id (a :: Account)) uName nm (role (a :: Account)) (level (a :: Account)) (settings (a :: Account)) accessToken
  where
    uName = T.toLower $ username (authReq :: LoginRequest)

    getTeacher subId isAdmin = do
        teachr <- runDB $ DB.getTeacherBySubjectId subId
        return (TeacherScope subId (schoolId (teachr :: Teacher)) isAdmin, name (teachr :: Teacher))

    createToken acct = do
        let subId = id (acct :: Account)
        (scope, nm) <- case userType (role (acct :: Account)) of
            "Student" -> do
                 stdnt <- runDB $ DB.getStudentBySubjectId subId
                 return (StudentScope subId (schoolId (stdnt :: Student)), name (stdnt :: Student))
            "Teacher" -> getTeacher subId False

            "SchoolAdmin" -> getTeacher subId True
            "Editor" -> return (EditorScope subId, "Anonymous")
        jwk <- fmap tokenKey ask
        token_ <- mkAccessToken jwk scope
        return (token_, nm)

accountServer :: DB db => ApiServer AccountApi db
accountServer token_ =
    case token_ of
        Nothing -> throwAll err401 :<|> registerNewAccount :<|> throwAll err401
        Just t -> updateSettings (scopeSubjectId t) :<|> throwAll err403 :<|> newRegistrationCode t
  where
    updateSettings userId newSettings = do
        runDB $ DB.updateAccountSettings (userId, newSettings)
        return NoContent

    registerNewAccount registration = do
        existing <- runDB $ DB.getAccountByUsername (email registration)
        unless (isNothing existing) $ throwError err409
        hashedPassword <- fmap decodeUtf8 $ liftIO $ hashPassword passwordOptions (encodeUtf8 (password (registration :: Registration)))
        result <- runDB $ DB.registerNewAccount (registration { password = hashedPassword } )
        case result of
            Just _ -> return NoContent
            Nothing -> throwError err403

    newRegistrationCode t = case t of
        TeacherScope _ schoolId_ True ->
            runDB $ DB.createRegistrationCode schoolId_
        _ -> throwError err403

    passwordOptions = defaultOptions { iterations = 3, parallelism = 1, memory = 4096}

storyServer :: DB db => ApiServer StoriesApi db
storyServer token_ =
    getStories :<|> specificStoryServer :<|> createStory
  where
    notFound = err404 { errBody = "Story with this ID was not found" }

    specificStoryServer storyId_ = getStory storyId_ :<|> updateStory storyId_

    getStories =
        case token_ of
            Nothing -> fmap sampleStories ask
            _ -> runDB DB.getStories

    getStory storyId_ = do
        story <- runDB (DB.getStory storyId_)
        case story of
            Nothing -> throwError notFound
            Just s -> return s

    updateStory storyId_ story =
        case token_ of
            Just (EditorScope _) -> runDB (DB.updateStory (story { id = storyId_} ))
            _ -> throwError err403

    createStory story = do
        newId <- runDB (DB.createStory story)
        return (story { id = newId } :: Story)


trailsServer :: DB db => ApiServer TrailsApi db
trailsServer Nothing = throwAll err401
trailsServer (Just (TeacherScope _ sid _)) =
    getTrailsForSchool sid :<|> createTrail
trailsServer (Just (StudentScope _ sid)) =
    getTrailsForSchool sid :<|> throwAll err403
trailsServer _ = throwAll err403

getTrailsForSchool :: DB db => SchoolId -> HandlerT db [StoryTrail]
getTrailsForSchool = runDB . DB.getTrails

createTrail :: DB db => StoryTrail -> HandlerT db StoryTrail
createTrail trail = do
    uuid <- liftIO (toText <$> nextRandom)
    let trailWithId = trail { id = uuid } :: StoryTrail
    _ <- runDB (DB.createTrail trailWithId)
    return trailWithId


schoolsServer :: DB db => ApiServer SchoolsApi db
schoolsServer Nothing = throwAll err401
schoolsServer (Just scp@(AdminScope _)) = runDB DB.getSchools :<|> specificSchoolServer scp
schoolsServer _ = throwAll err403


schoolServer :: DB db => ApiServer SchoolApi db
schoolServer scope = case scope of
    Nothing -> throwAll err401
    Just scp@(TeacherScope _ sid _) -> specificSchoolServer scp sid
    Just scp@(StudentScope _ sid) ->
        throwAll err403
        :<|> throwAll err403
        :<|> answersServer scp
        :<|> leaderBoardServer sid
        :<|> throwAll err403
    _ -> throwAll err403


specificSchoolServer :: DB db => AccessScope -> SchoolId -> ApiServer (ClassesApi :<|> StudentsApi :<|> AnswersApi :<|> LeaderBoardApi :<|> TeachersApi) db
specificSchoolServer scp sid = classesServer (scopeSubjectId scp) sid :<|> studentsServer scp sid :<|> answersServer scp :<|> leaderBoardServer sid :<|> teachersServer scp sid


classesServer :: DB db => SubjectId -> SchoolId -> ApiServer ClassesApi db
classesServer subId sid = runDB (DB.getClasses sid) :<|> specificClassServer :<|> createClass
  where
    specificClassServer cid = getClass cid :<|> deleteClass cid :<|> setClassMembers cid

    getClass cid = do
        c <- runDB (DB.getClass cid)
        maybe (throwError err404) return c

    deleteClass cid = do
         _ <- runDB (DB.deleteClass cid sid)
         return NoContent

    setClassMembers cid studentIds delete =
        if delete == Just True
            then
                runDB (DB.removeClassMembers sid cid studentIds)
            else
                runDB (DB.addClassMembers sid cid studentIds)

    createClass (nm, desc) = do
        uuid <- newUUID
        let c = Class uuid nm (Just desc) sid subId []
        result <- runDB (DB.createClass c)
        case result of
            Nothing -> throwError err409
            Just _ -> return c;


studentsServer :: DB db => AccessScope -> SchoolId -> ApiServer StudentsApi db
studentsServer scp schoolId_ = runDB (DB.getStudents schoolId_) :<|> specificStudentServer :<|> createStudents
  where
    specificStudentServer studId = getStudent studId :<|> updateStudent studId :<|> changePassword studId :<|> changeUsername studId :<|> studentAdmin studId scp

    studentAdmin _ (TeacherScope _ _ False) = throwAll err403
    studentAdmin studId _ = deleteStudent studId :<|> undelete studId

    getStudent studId = do
        s <- runDB $ DB.getStudent schoolId_ studId
        maybe (throwError err404) return s

    changePassword studId password_ = do
        logInfoN $ "Setting password for student: " <> studId
        when (T.length password_ < 8) (throwError err400)
        hashedPassword <- encodePassword password_
        runDB $ DB.setStudentPassword schoolId_ studId hashedPassword
        return NoContent

    changeUsername studId username_ = do
        logInfoN $ "Setting username for student: " <> studId <> " to " <> username_
        when (T.length username_ < 3) (throwError err400)

        user <- runDB $ DB.getAccountByUsername username_
        case user of
            Nothing -> runDB $ DB.setStudentUsername schoolId_ studId username_
            Just u ->
                unless (id (u :: Account) == studId) (throwError err409)
        return NoContent

    updateStudent _ student_ = runDB $ DB.updateStudent student_ schoolId_

    deleteStudent studId = runDB $ DB.deleteStudent studId schoolId_

    undelete studId = do
        runDB $ DB.undeleteStudent studId schoolId_
        return NoContent

    clean = T.filter (\c -> c /= ' ' && c /= '-' && c /= '\'')

    generatePassword minLength= do
        ws <- map clean <$> runDB DB.generateWords
        let wls = zip ws (map (Sum . T.length) ws)
            scan = scanl' (<>) mempty wls
            pass =
                case uncons (dropWhile ((< minLength) . snd) scan) of
                    Just ((p, _), _) -> p
                    Nothing -> fst (last scan)

        return (T.toLower pass)

    createStudents (level_, names) = mapM (createStudent level_) names

    createStudent level_ nm = do
        logInfoN $ "Creating new student account for: " <> nm
        uname <- runDB $ DB.generateUsername (T.toLower (clean nm))
        logInfoN $ "Generated username is: " <> uname
        pass <- generatePassword 15
        hashedPassword <- encodePassword pass

        stdnt <- runDB $ DB.createStudent (nm, level_, schoolId_) (uname, hashedPassword)
        return (stdnt, (uname, pass))

    encodePassword p = fmap decodeUtf8 $ liftIO $ hashPassword passwordOptions (encodeUtf8 p)

    passwordOptions = defaultOptions { iterations = 3, parallelism = 1, memory = 4096}

teachersServer :: DB db => AccessScope -> SchoolId -> ApiServer TeachersApi db
teachersServer (TeacherScope _ _ True) schoolId_ = runDB (DB.getTeachers schoolId_) :<|> activateAccount
  where
    activateAccount accountId = runDB (DB.activateAccount (schoolId_, accountId)) >> return accountId
teachersServer _ _ = throwAll err403



answersServer :: DB db => AccessScope -> ApiServer AnswersApi db
answersServer scope = case scope of
    TeacherScope _ schoolId_ _ -> getAnswers schoolId_ :<|> throwAll err403
    StudentScope subId schoolId_ -> getStudentAnswers subId schoolId_  :<|> createAnswer schoolId_ subId
    _ -> throwAll err403
  where
    createAnswer schId subId a = do
        let a_ = a { studentId = subId } :: Answer
        _ <- runDB $ DB.createAnswer (a_, schId)
        return a_

    -- Student can only query their answers or answers for a specific story,
    -- not all the stories for the school, or other student's answers.
    -- So, if a specific story hasn't been requested, we ignore the extra
    -- "student" parameter value and use the student's own identity instead.
    getStudentAnswers studentSubId schoolId_ storyId_ _ =
        case storyId_ of
            Nothing -> getAnswers schoolId_ Nothing (Just studentSubId)
            sid -> getAnswers schoolId_ sid Nothing

    getAnswers schId stId subId =
        runDB (DB.getAnswers schId subId stId)

leaderBoardServer :: DB db => SchoolId -> ApiServer LeaderBoardApi db
leaderBoardServer sid =
    runDB (DB.getLeaderBoard sid)

dictServer :: DB db => ApiServer DictApi db
dictServer =
    runDB DB.getDictionary :<|> runDB . DB.lookupWord

-- ThrowAll idea taken from servant-auth
class ThrowAll a where
    throwAll :: ServantErr -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
    throwAll e = throwAll e :<|> throwAll e

instance {-# OVERLAPS #-} ThrowAll b => ThrowAll (a -> b) where
    throwAll e = const $ throwAll e

instance {-# OVERLAPPABLE #-} (MonadError ServantErr m) => ThrowAll (m a) where
    throwAll = throwError
