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
import           Servant ((:<|>) ((:<|>)), Server, err401, err404, errBody, Handler)

import           Api.Types

server :: TVar DB -> Server Api
server tDb = storyServer tDb :<|> dictServer tDb :<|> schoolsServer :<|> loginServer

loginServer :: Server LoginApi
loginServer authReq = case lookup (username (authReq :: LoginRequest), password authReq) users of
    Nothing -> throwError err401
    Just r -> return r
  where
    users =
        [ (("admin", "admin"), Login "aid" "admin" "Dr Admin" admin (AccessToken "admin"))
        , (("editor", "editor"), Login "eid" "editor" "Mr Ed" editor (AccessToken "editor"))
        , (("teacher", "teacher"), Login "tid1" "teacher" "Captain Teach" teacher (AccessToken "teacher:4"))
        , (("mammy", "mammy"), Login "tid2" "mammy" "Mammy Two Shoes" teacher (AccessToken "teacher:3"))
        , (("jerry", "jerry"), Login "uid1" "jerry" "Jerry Mouse" student (AccessToken "student:3"))
        , (("tom", "tom"), Login "uid2" "tom" "Tom Cat" student (AccessToken "student:3"))
        , (("jack", "jack"), Login "uid3" "jack" "Jack Sparrow" student (AccessToken "student:4"))
        ]

schools :: [School]
schools =
    [ School "3" "T&J Academy"
    , School "4" "Pirate School"
    ]

classes :: [Class]
classes =
    [ Class "1" "A1" "3" []
    , Class "2" "A2" "3" []
    , Class "3" "B1" "3" []
    , Class "4" "P1" "4" []
    , Class "5" "P2" "4" []
    ]

students :: [Student]
students =
    [ Student "1" "Jerry Mouse" "3"
    , Student "2" "Tom Cat" "3"
    , Student "3" "Butch" "3"
    , Student "4" "Nibbles" "3"
    , Student "5" "Tyke" "3"
    , Student "6" "Jack Sparrow" "4"
    ]

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

    classesInSchool sid = filter (\c -> schoolId (c :: Class) == sid) classes


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
