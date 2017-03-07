module Api.Server
    ( server
    ) where

import           Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Prelude hiding (id)
import           Servant ((:<|>) ((:<|>)), Server)

import           Api.Types (Api, Story (..), DB (..))


server :: TVar DB -> Server Api
server tDb = getStories :<|> createStory
  where
    getStories =
        liftIO . atomically $ do
            db <- readTVar tDb
            return (Map.elems (stories db))

    createStory story = do
        uuid <- liftIO (toText <$> nextRandom)
        liftIO . atomically $ do
            db <- readTVar tDb
            let storyWithId = story { id = Just uuid }
                newStories  = Map.insert uuid storyWithId (stories db)
            writeTVar tDb db { stories = newStories }
            return storyWithId
