{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api.Server
    ( server
    ) where

import           Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Except (throwError)
import           Data.List (find)
import qualified Data.Map.Strict as Map
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Prelude hiding (id)
import qualified Prelude
import           Servant ((:<|>) ((:<|>)), Server, err404, errBody, Handler)

import           Api.Types

server :: TVar DB -> Server Api
server tDb = storyServer tDb :<|> dictServer tDb :<|> schoolsServer

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
            let storyWithId = story { id = Just uuid } :: Story
                newStories  = Map.insert uuid storyWithId (stories db)
            writeTVar tDb db { stories = newStories }
            return storyWithId

    withStories f =
        liftIO . atomically $ do
            db <- readTVar tDb
            return (f (stories db))

schoolsServer :: Server SchoolsApi
schoolsServer = getSchools :<|> serveSchool
  where
    getSchools = return schools

    serveSchool sid = getClasses sid :<|> getClass sid

    getClasses :: SchoolId -> Handler [Class]
    getClasses sid = return (classesInSchool sid)

    getClass :: SchoolId -> ClassId -> Handler Class
    getClass sid cid =
        maybe (throwError err404) return $ find (\c -> id (c :: Class) == cid) (classesInSchool sid)

    schools =
        [ School "1" "Dog High"
        , School "2" "Cat School"
        , School "3" "Mouse Academy"
        ]

    classesInSchool sid = filter (\c -> schoolId (c :: Class) == sid) classes

    classes :: [Class]
    classes =
        [ Class "1" "A1" "3" []
        , Class "2" "A2" "3" []
        , Class "3" "B1" "3" []
        , Class "4" "A1" "2" []
        , Class "5" "A1" "1" []
        ]

    students =
        [ Student "1" "Jerry Mouse" "3"
        , Student "2" "Tom Cat" "2"
        , Student "3" "Butch the Dog" "1"
        ]

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
