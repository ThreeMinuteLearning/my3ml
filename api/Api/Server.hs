{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.Server
    ( server
    ) where

import           Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Prelude hiding (id)
import qualified Prelude
import           Servant ((:<|>) ((:<|>)), Server, err404, errBody)

import           Api.Types


server :: TVar DB -> Server Api
server tDb = storyServer tDb :<|> dictServer tDb

storyServer :: TVar DB -> Server StoriesApi
storyServer tDb =
    getStories :<|> getStory :<|> createStory
  where
    notFound = err404 { errBody = "Story with this ID was not found" }

    getStories = withStories Map.elems

    getStory storyId = do
        story <- withStories (Map.lookup storyId)
        case story of
            Nothing -> throwError notFound
            Just s -> return s

    createStory story = do
        uuid <- liftIO (toText <$> nextRandom)
        liftIO . atomically $ do
            db <- readTVar tDb
            let storyWithId = story { id = Just uuid }
                newStories  = Map.insert uuid storyWithId (stories db)
            writeTVar tDb db { stories = newStories }
            return storyWithId

    withStories f =
        liftIO . atomically $ do
            db <- readTVar tDb
            return (f (stories db))

dictServer :: TVar DB -> Server DictApi
dictServer tDb =
    getDictionary :<|> getWord
  where
    getWord w = do
        defs <- withDict (Map.lookup w)
        case defs of
            Nothing -> throwError err404
            Just ds -> return ds

    getDictionary = withDict Prelude.id

    withDict f =
       liftIO . atomically $ do
           db <- readTVar tDb
           return (f (dictionary db))
