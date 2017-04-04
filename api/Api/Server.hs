{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Server
    ( server
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Except (MonadError, throwError)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Prelude hiding (id)
import           Servant ((:<|>) ((:<|>)), Server, ServantErr, err401, err403, err404, errBody, Handler)

import           Api.Auth (AccessScope(..))
import           Api.Types hiding (AccessToken)
import qualified DB


server :: DB.DB backend => backend -> Server Api
server db = storyServer db :<|> dictServer db :<|> schoolsServer db :<|> schoolServer db :<|> trailsServer db :<|> loginServer


loginServer :: Server LoginApi
loginServer authReq = case lookup (username (authReq :: LoginRequest), password (authReq :: LoginRequest)) users of
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


storyServer :: DB.DB backend => backend -> Server StoriesApi
storyServer db token_ =
    getStories :<|> getStory :<|> createStory
  where
    notFound = err404 { errBody = "Story with this ID was not found" }

    getStories :: Handler [Story]
    getStories =
        case token_ of
            Nothing -> return []
            _ -> DB.getStories db

    getStory storyId = do
        story <- DB.getStory db storyId
        case story of
            Nothing -> throwError notFound
            Just s -> return s

    createStory story = do
        uuid <- liftIO (toText <$> nextRandom)
        let storyWithId = story { id = Just uuid } :: Story
        _ <- DB.createStory db storyWithId
        return storyWithId


trailsServer :: DB.DB backend => backend -> Server TrailsApi
trailsServer _ Nothing = throwAll err401
trailsServer db (Just (TeacherScope sid)) =
    DB.getTrails db sid :<|> createTrail db
trailsServer db (Just (StudentScope _ sid)) =
    DB.getTrails db sid :<|> throwAll err403
trailsServer _ _ = throwAll err403


createTrail :: DB.DB backend => backend -> StoryTrail -> Handler StoryTrail
createTrail db trail = do
    uuid <- liftIO (toText <$> nextRandom)
    let trailWithId = trail { id = uuid } :: StoryTrail
    _ <- DB.createTrail db trailWithId
    return trailWithId


schoolsServer :: DB.DB backend => backend -> Server SchoolsApi
schoolsServer _ Nothing = throwAll err401
schoolsServer db (Just AdminScope) = DB.getSchools db :<|> specificSchoolServer db
schoolsServer _ _ = throwAll err403


schoolServer :: DB.DB backend => backend -> Server SchoolApi
schoolServer _ Nothing = throwAll err401
schoolServer db (Just (TeacherScope sid)) = specificSchoolServer db sid
schoolServer _ _ = throwAll err403


specificSchoolServer :: DB.DB backend => backend -> SchoolId -> Server (ClassesApi :<|> StudentsApi)
specificSchoolServer db sid = classesServer db sid :<|> studentsServer db sid


classesServer :: DB.DB backend => backend -> SchoolId -> Server ClassesApi
classesServer db sid = DB.getClasses db sid :<|> getClass
  where
    getClass :: ClassId -> Handler Class
    getClass cid = do
        c <- DB.getClass db cid
        maybe (throwError err404) return c


studentsServer :: DB.DB backend => backend -> SchoolId -> Server StudentsApi
studentsServer db schoolId_ = DB.getStudents db schoolId_ :<|> getStudent :<|> mapM createStudent
  where
    getStudent :: StudentId -> Handler Student
    getStudent studentId = do
        s <- DB.getStudent db schoolId_ studentId
        maybe (throwError err404) return s

    createStudent nm = do
        uuid <- liftIO (toText <$> nextRandom)
        return (Student uuid nm Nothing 5 "" schoolId_, ("username", "password"))

dictServer :: DB.DB backend => backend -> Server DictApi
dictServer db =
    DB.getDictionary db :<|> DB.lookupWord db

-- ThrowAll idea taken from servant-auth
class ThrowAll a where
    throwAll :: ServantErr -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
    throwAll e = throwAll e :<|> throwAll e

instance {-# OVERLAPS #-} ThrowAll b => ThrowAll (a -> b) where
    throwAll e = const $ throwAll e

instance {-# OVERLAPPABLE #-} (MonadError ServantErr m) => ThrowAll (m a) where
    throwAll = throwError
