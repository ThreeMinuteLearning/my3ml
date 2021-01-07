{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstraintKinds #-}

module DB where

import           Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Aeson (Value)
import           Data.ByteString (ByteString)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Data.Maybe (fromMaybe)
import           Data.List (find)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import           GHC.Stack.Types (HasCallStack)
import           Prelude hiding (id)

import Api.Types

type DBFn m = (MonadIO m, HasCallStack)

class DB db where
    registerNewAccount :: DBFn m => Registration -> UserKeys -> Maybe ByteString -> db -> m (Maybe ())

    createRegistrationCode :: DBFn m => SchoolId -> db -> m Text

    getUserKeys :: DBFn m => SubjectId -> db -> m (Maybe UserKeys)

    activateAccount :: DBFn m => (SchoolId, SubjectId) -> ByteString -> db -> m ()

    loginSuccess :: DBFn m => SubjectId -> Maybe ByteString -> db -> m ()

    getAccountByUsername :: DBFn m => Text -> db -> m (Maybe Account)

    getStories :: DBFn m => Bool -> db -> m StoryData

    getStory :: DBFn m => StoryId -> db -> m (Maybe Story)

    createStory :: DBFn m => Story -> db -> m StoryId

    updateStory :: DBFn m => Story -> db -> m Story

    getSchools :: DBFn m => db -> m [School]

    getSchool :: DBFn m => SchoolId -> db -> m (Maybe School)

    getAnthologies :: DBFn m => Maybe SchoolId -> db -> m [Anthology]

    createAnthology :: DBFn m => Anthology -> db -> m ()

    updateAnthology :: DBFn m => Anthology -> db -> m ()

    deleteAnthology :: DBFn m => AnthologyId -> Maybe SchoolId -> db -> m ()

    getAnthologyStories :: DBFn m => AnthologyId -> db -> m [Story]

    getStarterStories :: DBFn m => db -> m [Story]

    setStarterStories :: DBFn m => AnthologyId -> db -> m ()

    getClasses :: DBFn m => SchoolId -> db -> m [Class]

    getClass :: DBFn m => ClassId -> db -> m (Maybe Class)

    addClassMembers :: DBFn m => SchoolId -> ClassId -> [SubjectId] -> db -> m Class

    removeClassMembers :: DBFn m => SchoolId -> ClassId -> [SubjectId] -> db -> m Class

    createClass :: DBFn m => Class -> db -> m (Maybe ())

    deleteClass :: DBFn m => ClassId -> SchoolId -> db -> m ()

    getStudents :: DBFn m => SchoolId -> db -> m [Student]

    getStudent :: DBFn m => SchoolId -> SubjectId -> db -> m (Maybe Student)

    getStudentBySubjectId :: DBFn m => SubjectId -> db -> m Student

    updateStudent :: DBFn m => Student -> SchoolId -> db -> m Student

    createStudent :: DBFn m => (Text, Int, SchoolId) -> (Text, Text) -> db -> m Student

    deleteStudent :: DBFn m => SubjectId -> SchoolId -> db -> m Student

    undeleteStudent :: DBFn m => SubjectId -> SchoolId -> db -> m ()

    setStudentPassword :: DBFn m => SchoolId -> SubjectId -> Text -> db -> m ()

    setStudentUsername :: DBFn m => SchoolId -> SubjectId -> Text -> db -> m ()

    getTeacherBySubjectId :: DBFn m => SubjectId -> db -> m Teacher

    getTeachers :: DBFn m => SchoolId -> db -> m [(Teacher, Bool)]

    getDictionary :: DBFn m => db -> m WordDictionary

    lookupWord :: DBFn m => Text -> db -> m [WordDefinition]

    getAnswers :: DBFn m => SchoolId -> Maybe SubjectId -> Maybe StoryId -> db -> m [Answer]

    createAnswer :: DBFn m => (Answer, SchoolId) -> db -> m ()

    generateWords :: DBFn m => db -> m [Text]

    generateUsername :: DBFn m => db -> m Text

    getLeaderBoard :: DBFn m => SchoolId -> db -> m [LeaderBoardEntry]

    updateAccountSettings :: DBFn m => (SubjectId, Value) -> db -> m ()

    getDashboard :: DBFn m => db -> m Value

data InMemoryDB = InMemoryDB
    { stories :: Map.Map StoryId Story
    , sampleStories :: [Story]
    , dictionary :: WordDictionary
    , anthologies :: [Anthology]
    , schools :: [School]
    , classes :: [Class]
    , students :: [Student]
    }

newtype AtomicDB = AtomicDB (TVar InMemoryDB)

mkDB :: InMemoryDB -> IO AtomicDB
mkDB db = AtomicDB <$> newTVarIO db

withDB :: MonadIO m => AtomicDB -> (InMemoryDB -> a) -> m a
withDB (AtomicDB tDb) f =
    liftIO . atomically $ do
        db <- readTVar tDb
        return $ f db

updateDB :: MonadIO m => AtomicDB -> (InMemoryDB -> InMemoryDB) -> m ()
updateDB (AtomicDB tDB) f =
    liftIO . atomically $ do
        db <- readTVar tDB
        writeTVar tDB (f db)

newUUID :: MonadIO m => m Text
newUUID = liftIO (toText <$> nextRandom)

instance DB AtomicDB where
    getStories _ db = do
        dbStories <- Map.elems <$> withDB db stories
        return $ StoryData dbStories []

    getStory sid db = Map.lookup sid <$> withDB db stories

    createStory story db = do
        updateDB db $ \d ->
            let newStories = Map.insert 999 story (stories (d :: InMemoryDB))
            in  d { stories = newStories }
        return 1

    getAnthologies _ db = withDB db anthologies

    createAnthology anthology db =
        updateDB db $ \d ->
            let newAnthologies = anthology : anthologies d
            in  d { anthologies = newAnthologies }

    deleteAnthology trid _ db =
        updateDB db $ \d ->
            let newAnthologies = filter (\t -> id (t :: Anthology) /= trid) (anthologies d)
            in  d { anthologies = newAnthologies }

    getSchools db = withDB db schools

    getSchool schoolId_ db = find (\s -> id (s :: School) == schoolId_) <$> withDB db schools

    getClasses sid db = filter (\c -> schoolId (c :: Class) == sid) <$> withDB db classes

    getClass cid db = find (\c -> id (c :: Class) == cid) <$> withDB db classes
    getStudents sid db = filter (\s -> schoolId (s :: Student) == sid) <$> withDB db (students :: InMemoryDB -> [Student])

    getStudent schoolId_ studentId_ db = do
        studs <- getStudents schoolId_ db
        return $ find (\s -> id (s :: Student) == studentId_) studs

    createStudent (nm, lvl, schoolId_) creds db = do
        uuid <- newUUID
        createdAt <- liftIO getPOSIXTime
        let s = Student (SubjectId uuid) nm Nothing lvl schoolId_ False Nothing createdAt
        updateDB db $ \d ->
            let newStudents = s : students (d :: InMemoryDB)
            in  d { students = newStudents }
        return s

    getDictionary db = withDB db dictionary

    lookupWord w db = (fromMaybe [] . Map.lookup w) <$> withDB db dictionary
