{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Server
    ( server
    , DB (..)
    ) where

import           Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Except (MonadError, throwError)
import           Data.List (find)
import qualified Data.Map.Strict as Map
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Prelude hiding (id)
import qualified Prelude
import           Servant ((:<|>) ((:<|>)), Server, ServantErr, err401, err403, err404, errBody, Handler)

import           Api.Auth (AccessScope(..))
import           Api.Types hiding (AccessToken)

data DB = DB
    { stories :: Map.Map StoryId Story
    , starterStories :: [Story]
    , dictionary :: WordDictionary
    }

server :: TVar DB -> Server Api
server tDb = storyServer tDb :<|> dictServer tDb :<|> schoolsServer :<|> schoolServer :<|> loginServer

loginServer :: Server LoginApi
loginServer authReq = case lookup (username (authReq :: LoginRequest), password authReq) users of
    Nothing -> throwError err401
    Just r -> return r
  where
    users =
        [ (("admin", "admin"), Login "aid" "admin" "Dr Admin" admin "admin")
        , (("editor", "editor"), Login "eid" "editor" "Mr Ed" editor "editor")
        , (("teacher", "teacher"), Login "tid1" "teacher" "Captain Teach" teacher "t:4")
        , (("mammy", "mammy"), Login "tid2" "mammy" "Mammy Two Shoes" teacher "t:3")
        , (("jerry", "jerry"), Login "uid1" "jerry" "Jerry Mouse" student "s:1:3")
        , (("tom", "tom"), Login "uid2" "tom" "Tom Cat" student "s:2:3")
        , (("jack", "jack"), Login "uid3" "jack" "Jack Sparrow" student "s:6:4")
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

allStudents :: [Student]
allStudents =
    [ Student "1" "Jerry Mouse" 5 "3"
    , Student "2" "Tom Cat" 3 "3"
    , Student "3" "Butch" 3 "3"
    , Student "4" "Nibbles" 2 "3"
    , Student "5" "Tyke" 2 "3"
    , Student "6" "Jack Sparrow" 8 "4"
    ]

storyServer :: TVar DB -> Server StoriesApi
storyServer tDb token_ =
    getStories :<|> getStory :<|> createStory
  where
    notFound = err404 { errBody = "Story with this ID was not found" }

    getStories =
        liftIO . atomically $ do
            db <- readTVar tDb
            return $ case token_ of
                Nothing -> starterStories db
                _ -> Map.elems (stories db)

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
schoolsServer Nothing = throwAll err401
schoolsServer (Just AdminScope) = return schools :<|> specificSchoolServer
schoolsServer _ = throwAll err403

schoolServer :: Server SchoolApi
schoolServer Nothing = throwAll err401
schoolServer (Just (TeacherScope sid)) = specificSchoolServer sid
schoolServer _ = throwAll err403

specificSchoolServer :: SchoolId -> Server (ClassesApi :<|> StudentsApi)
specificSchoolServer sid = classesServer sid :<|> studentsServer sid

classesServer :: SchoolId -> Server ClassesApi
classesServer sid = getClasses :<|> getClass
  where
    getClasses :: Handler [Class]
    getClasses = return classesInSchool

    getClass :: ClassId -> Handler Class
    getClass cid =
        maybe (throwError err404) return $ find (\c -> id (c :: Class) == cid) classesInSchool

    classesInSchool = filter (\c -> schoolId (c :: Class) == sid) classes


studentsServer :: SchoolId -> Server StudentsApi
studentsServer schoolId_ = getStudents :<|> getStudent
  where
    getStudents :: Handler [Student]
    getStudents = return studentsInSchool

    getStudent :: StudentId -> Handler Student
    getStudent studentId =
        maybe (throwError err404) return $ find (\s -> id (s :: Student) == studentId) studentsInSchool

    studentsInSchool = filter (\s -> schoolId (s :: Student) == schoolId_) allStudents

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


-- ThrowAll idea taken from servant-auth
class ThrowAll a where
    throwAll :: ServantErr -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
    throwAll e = throwAll e :<|> throwAll e

instance {-# OVERLAPS #-} ThrowAll b => ThrowAll (a -> b) where
    throwAll e = const $ throwAll e

instance {-# OVERLAPPABLE #-} (MonadError ServantErr m) => ThrowAll (m a) where
    throwAll = throwError
