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
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Jose.Jwk
import           Prelude hiding (id)
import           Servant ((:<|>) ((:<|>)), ServerT, ServantErr, Handler, NoContent(..), err400, err401, err403, err404, errBody)

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

    createToken acct = do
        let subId = id (acct :: Account)
        (scope, nm) <- case userType (role (acct :: Account)) of
            "Student" -> do
                 stdnt <- runDB $ DB.getStudentBySubjectId subId
                 return (StudentScope subId (schoolId (stdnt :: Student)), name (stdnt :: Student))
            "Teacher" -> do
                 teachr <- runDB $ DB.getTeacherBySubjectId subId
                 return (TeacherScope subId (schoolId (teachr :: Teacher)), name (teachr :: Teacher))
        jwk <- fmap tokenKey ask
        token_ <- mkAccessToken jwk scope
        return (token_, nm)


storyServer :: DB db => ApiServer StoriesApi db
storyServer token_ =
    getStories :<|> getStory :<|> createStory
  where
    notFound = err404 { errBody = "Story with this ID was not found" }

    getStories =
        case token_ of
            Nothing -> fmap sampleStories ask
            _ -> runDB DB.getStories

    getStory storyId_ = do
        story <- runDB (DB.getStory storyId_)
        case story of
            Nothing -> throwError notFound
            Just s -> return s

    createStory story = do
        uuid <- liftIO (toText <$> nextRandom)
        let storyWithId = story { id = uuid } :: Story
        _ <- runDB (DB.createStory storyWithId)
        return storyWithId


trailsServer :: DB db => ApiServer TrailsApi db
trailsServer Nothing = throwAll err401
trailsServer (Just (TeacherScope _ sid)) =
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
schoolServer Nothing = throwAll err401
schoolServer (Just scp@(TeacherScope _ sid)) = specificSchoolServer scp sid
schoolServer (Just scp@(StudentScope _ _)) = throwAll err403 :<|> throwAll err403 :<|> answersServer scp
schoolServer _ = throwAll err403


specificSchoolServer :: DB db => AccessScope -> SchoolId -> ApiServer (ClassesApi :<|> StudentsApi :<|> AnswersApi) db
specificSchoolServer scp sid = classesServer (scopeSubjectId scp) sid :<|> studentsServer sid :<|> answersServer scp


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


studentsServer :: DB db => SchoolId -> ApiServer StudentsApi db
studentsServer schoolId_ = runDB (DB.getStudents schoolId_) :<|> specificStudentServer :<|> mapM createStudent
  where
    specificStudentServer studId = getStudent studId :<|> updateStudent studId :<|> deleteStudent studId :<|> changePassword studId :<|> changeUsername studId :<|> undelete studId

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
        runDB $ DB.setStudentUsername schoolId_ studId username_
        return NoContent

    updateStudent _ student_ = runDB $ DB.updateStudent student_ schoolId_

    deleteStudent studId = runDB $ DB.deleteStudent studId schoolId_

    undelete studId = do
        runDB $ DB.undeleteStudent studId schoolId_
        return NoContent

    generateUsername nm = return nm

    generatePassword = return "password"

    createStudent nm = do
        logInfoN $ "Creating new student account: " <> nm
        username <- generateUsername nm
        password <- generatePassword

        let creds = (username, password)
        stdnt <- runDB $ DB.createStudent (nm, 5, schoolId_) creds
        return (stdnt, creds)


answersServer :: DB db => AccessScope -> ApiServer AnswersApi db
answersServer scope = case scope of
    TeacherScope _ schoolId_ -> getAnswers schoolId_ :<|> throwAll err403
    StudentScope subId schoolId_ -> getStudentAnswers subId schoolId_  :<|> createAnswer schoolId_ subId
    _ -> throwAll err403
  where
    createAnswer schId subId a = do
        uuid <- newUUID
        let a_ = a { id = uuid, studentId = subId } :: Answer
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
