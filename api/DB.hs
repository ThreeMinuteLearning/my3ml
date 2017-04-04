{-# LANGUAGE DuplicateRecordFields #-}

module DB where

import           Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Maybe (fromMaybe)
import           Data.List (find)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Prelude hiding (id)

import Api.Types

class DB backend where
    getStories :: MonadIO m => backend -> m [Story]

    getSampleStories :: MonadIO m => backend -> m [Story]

    getStory :: MonadIO m => backend -> StoryId -> m (Maybe Story)

    createStory :: MonadIO m => backend -> Story -> m Story

    getSchools :: MonadIO m => backend -> m [School]

    getSchool :: MonadIO m => backend -> SchoolId -> m (Maybe School)

    getTrails :: MonadIO m => backend -> SchoolId -> m [StoryTrail]

    createTrail :: MonadIO m => backend -> StoryTrail -> m StoryTrail

    deleteTrail :: MonadIO m => backend -> TrailId -> m (Either String ())

    getClasses :: MonadIO m => backend -> SchoolId -> m [Class]

    getClass :: MonadIO m => backend -> ClassId -> m (Maybe Class)

    getStudents :: MonadIO m => backend -> SchoolId -> m [Student]

    getStudent :: MonadIO m => backend -> SchoolId -> StudentId -> m (Maybe Student)

    getDictionary :: MonadIO m => backend -> m WordDictionary

    lookupWord :: MonadIO m => backend -> Text -> m [WordDefinition]


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

storyId :: Story -> Text
storyId = id :: Story -> Text

instance DB AtomicDB where
    getStories db = Map.elems <$> withDB db stories

    getSampleStories db = withDB db sampleStories

    getStory db sid = Map.lookup sid <$> withDB db stories

    createStory db story = do
        updateDB db $ \d ->
            let newStories = Map.insert (storyId story) story (stories (d ::InMemoryDB))
            in  d { stories = newStories }
        return story

    getTrails db sid = filter (\t -> schoolId (t :: StoryTrail) == sid) <$> withDB db trails

    createTrail db trail = do
        updateDB db $ \d ->
            let newTrails = trail : trails d
            in  d { trails = newTrails }
        return trail

    deleteTrail db trid = do
        updateDB db $ \d ->
            let newTrails = filter (\t -> id (t :: StoryTrail) /= trid) (trails d)
            in  d { trails = newTrails }
        return $ Right ()

    getSchools db = withDB db schools

    getSchool db schoolId_ = find (\s -> id (s :: School) == schoolId_) <$> withDB db schools

    getClasses db sid = filter (\c -> schoolId (c :: Class) == sid) <$> withDB db classes

    getClass db cid = find (\c -> id (c :: Class) == cid) <$> withDB db classes
    getStudents db sid = filter (\s -> schoolId (s :: Student) == sid) <$> withDB db (students :: InMemoryDB -> [Student])

    getStudent db schoolId_ studentId_ = do
        studs <- getStudents db schoolId_
        return $ find (\s -> id (s :: Student) == studentId_) studs

    getDictionary db = withDB db dictionary

    lookupWord db w = (fromMaybe [] . Map.lookup w) <$> withDB db dictionary
