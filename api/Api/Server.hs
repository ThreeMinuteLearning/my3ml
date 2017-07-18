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
import           Data.Monoid ((<>), Sum(..))
import           Data.List (scanl', last, uncons)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Jose.Jwk
import           Prelude hiding (id)
import           Servant ((:<|>) ((:<|>)), ServerT, ServantErr, Handler, NoContent(..), err400, err401, err403, err409, err404, errBody)

import           Api.Auth (AccessScope(..), mkAccessToken, scopeSubjectId)
import           Api.Types hiding (AccessToken)
import           DB (DB)
import qualified DB

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
server = storyServer :<|> dictServer :<|> schoolsServer :<|> schoolServer :<|> trailsServer :<|> loginServer

newUUID :: HandlerT db Text
newUUID = liftIO (toText <$> nextRandom)

loginServer :: DB db => ApiServer LoginApi db
loginServer authReq = do
    logInfoN $ "Login request from: " <> uName
    user <- runDB $ DB.getAccountByUsername uName
    case user of
        Nothing -> logInfoN ("User not found: " <> uName) >> throwError err401
        Just a -> do
            unless (validatePassword (password (a :: Account)) (password (authReq :: LoginRequest)))
                (throwError err401)
            (accessToken, nm) <- createToken a

            return $ Login (id (a :: Account)) uName nm (role (a :: Account)) (level (a :: Account)) accessToken
  where
    uName = T.toLower $ username (authReq :: LoginRequest)

    validatePassword passwd encodedPasswd = passwd == encodedPasswd

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
        uuid <- liftIO (toText <$> nextRandom)
        let storyWithId = story { id = uuid } :: Story
        _ <- runDB (DB.createStory storyWithId)
        return storyWithId


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
    _ -> throwAll err403


specificSchoolServer :: DB db => AccessScope -> SchoolId -> ApiServer (ClassesApi :<|> StudentsApi :<|> AnswersApi :<|> LeaderBoardApi) db
specificSchoolServer scp sid = classesServer (scopeSubjectId scp) sid :<|> studentsServer scp sid :<|> answersServer scp :<|> leaderBoardServer sid


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
        _ <- runDB (DB.createClass c)
        return c


studentsServer :: DB db => AccessScope -> SchoolId -> ApiServer StudentsApi db
studentsServer scp schoolId_ = runDB (DB.getStudents schoolId_) :<|> specificStudentServer :<|> mapM createStudent
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
        runDB $ DB.setStudentPassword schoolId_ studId password_
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

    createStudent nm = do
        logInfoN $ "Creating new student account for: " <> nm
        uname <- runDB $ DB.generateUsername (T.toLower (clean nm))
        logInfoN $ "Generated username is: " <> uname
        pass <- generatePassword 15

        let creds = (uname, pass)
        stdnt <- runDB $ DB.createStudent (uname, 5, schoolId_) creds
        return (stdnt, creds)


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
