{-# LANGUAGE DuplicateRecordFields #-}

module DB where

import           Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Data.Maybe (fromMaybe)
import           Data.List (find)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Prelude hiding (id)

import Api.Types

class DB db where
    getAccountByUsername :: MonadIO m => Text -> db -> m (Maybe Account)

    getStories :: MonadIO m => db -> m [Story]

    getStory :: MonadIO m => StoryId -> db -> m (Maybe Story)

    createStory :: MonadIO m => Story -> db -> m ()

    getSchools :: MonadIO m => db -> m [School]

    getSchool :: MonadIO m => SchoolId -> db -> m (Maybe School)

    getTrails :: MonadIO m => SchoolId -> db -> m [StoryTrail]

    createTrail :: MonadIO m => StoryTrail -> db -> m ()

    deleteTrail :: MonadIO m => TrailId -> db -> m ()

    getClasses :: MonadIO m => SchoolId -> db -> m [Class]

    getClass :: MonadIO m => ClassId -> db -> m (Maybe Class)

    createClass :: MonadIO m => Class -> db -> m ()

    getStudents :: MonadIO m => SchoolId -> db -> m [Student]

    getStudent :: MonadIO m => SchoolId -> SubjectId -> db -> m (Maybe Student)

    getStudentBySubjectId :: MonadIO m => SubjectId -> db -> m Student

    createStudent :: MonadIO m => (Text, Int, SchoolId) -> (Text, Text) -> db -> m Student

    getTeacherBySubjectId :: MonadIO m => SubjectId -> db -> m Teacher

    getDictionary :: MonadIO m => db -> m WordDictionary

    lookupWord :: MonadIO m => Text -> db -> m [WordDefinition]

    getAnswers :: MonadIO m => SchoolId -> Maybe SubjectId -> db -> m [Answer]

    createAnswer :: MonadIO m => (Answer, SchoolId) -> db -> m ()

data InMemoryDB = InMemoryDB
    { stories :: Map.Map StoryId Story
    , sampleStories :: [Story]
    , dictionary :: WordDictionary
    , trails :: [StoryTrail]
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
    getStories db =  Map.elems <$> withDB db stories

    getStory sid db = Map.lookup sid <$> withDB db stories

    createStory story db =
        updateDB db $ \d ->
            let newStories = Map.insert (id (story :: Story)) story (stories (d :: InMemoryDB))
            in  d { stories = newStories }

    getTrails sid db = filter (\t -> schoolId (t :: StoryTrail) == sid) <$> withDB db trails

    createTrail trail db =
        updateDB db $ \d ->
            let newTrails = trail : trails d
            in  d { trails = newTrails }

    deleteTrail trid db =
        updateDB db $ \d ->
            let newTrails = filter (\t -> id (t :: StoryTrail) /= trid) (trails d)
            in  d { trails = newTrails }

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
        let s = Student uuid nm Nothing lvl schoolId_
        updateDB db $ \d ->
            let newStudents = s : students (d :: InMemoryDB)
            in  d { students = newStudents }
        return s

    getDictionary db = withDB db dictionary

    lookupWord w db = (fromMaybe [] . Map.lookup w) <$> withDB db dictionary