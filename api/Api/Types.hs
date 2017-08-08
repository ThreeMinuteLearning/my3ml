{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Types where

import           Data.Aeson (Value, FromJSON, ToJSON)
import           Data.Time.Clock (UTCTime)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Elm (ElmType)
import           GHC.Generics (Generic)
import           Prelude hiding (id)
import           Servant ((:<|>), (:>), AuthProtect, Capture, QueryParam, ReqBody, Delete, Post, PostNoContent, NoContent, Get, JSON)

data Story = Story
    { id :: StoryId
    , title :: Text
    , img :: Text
    , level :: Int
    , qualification :: Text
    , tags :: [Text]
    , content :: Text
    , words :: [DictEntry]
    , clarifyWord :: Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data DictEntry = DictEntry
    { word :: Text
    , index :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type StoryId = Int

type WordDefinition = (Text, [(Text, Int)])

type WordDictionary = Map.Map Text [WordDefinition]

data School = School
    { id :: SchoolId
    , name :: Text
    , description :: Maybe Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type SchoolId = Text

data Class = Class
    { id :: ClassId
    , name :: Text
    , description :: Maybe Text
    , schoolId :: SchoolId
    , createdBy :: SubjectId
    , students :: [SubjectId]
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type ClassId = Text

data Answer = Answer
    { storyId :: StoryId
    , studentId :: SubjectId
    , connect :: Text
    , question :: Text
    , summarise :: Text
    , clarify :: Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Teacher = Teacher
    { id :: SubjectId
    , name :: Text
    , bio :: Maybe Text
    , schoolId :: SchoolId
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Student = Student
    { id :: SubjectId
    , name :: Text
    , description :: Maybe Text
    , level :: Int
    , schoolId :: SchoolId
    , hidden :: Bool
    , deleted :: Maybe UTCTime
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data LoginRequest = LoginRequest
    { username :: Text
    , password :: Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data StoryTrail = StoryTrail
    { id :: TrailId
    , name :: Text
--    , createdBy :: SubjectId
    , schoolId :: SchoolId
    , stories :: [StoryId]
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type TrailId = Text

data Account = Account
    { id :: SubjectId
    , username :: Text
    , password :: Text
    , role :: UserType
    , level :: Int
    , settings :: Maybe Value
    }

data Login = Login
    { sub :: SubjectId
    , username :: Text
    , name :: Text
    , role :: UserType
    , level :: Int
    , settings :: Maybe Value
    , token :: AccessToken
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data LeaderBoardEntry = LeaderBoardEntry
    { position :: Int
    , name :: Text
    , studentId :: SubjectId
    , score :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type SubjectId = Text

type AccessToken = Text

-- Change this to an ADT when elm-export support lands
newtype UserType = UserType {userType :: Text }
    deriving (Show, Generic, ElmType, ToJSON, FromJSON)

student, teacher, schoolAdmin, editor, admin :: UserType
student = UserType "Student"
teacher = UserType "Teacher"
schoolAdmin = UserType "SchoolAdmin"
editor = UserType "Editor"
admin = UserType "Admin"

type AccessTokenAuth = AuthProtect "access-token"

type LoginApi =
    "authenticate" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Login

type AccountApi =
    "account" :> "settings" :> AccessTokenAuth :> ReqBody '[JSON] Value :> Post '[JSON] NoContent

type StoriesApi =
    "stories" :> AccessTokenAuth :>
        (    Get '[JSON] [Story]
        :<|> Capture "storyId" StoryId :>
             (    Get '[JSON] Story
             :<|> ReqBody '[JSON] Story :> Post '[JSON] Story
             )
        :<|> ReqBody '[JSON] Story :> Post '[JSON] Story
        )

type DictApi =
    "dictionary" :>
        (    Get '[JSON] WordDictionary
        :<|> Capture "word" Text :> Get '[JSON] [WordDefinition]
        )

type SchoolsApi =
    "schools" :> AccessTokenAuth :>
        (    Get '[JSON] [School]
        :<|> Capture "schoolId" SchoolId :>
             (    ClassesApi
             :<|> StudentsApi
             :<|> AnswersApi
             :<|> LeaderBoardApi
             )
        )

type SchoolApi =
    "school" :> AccessTokenAuth :>
        (    ClassesApi
        :<|> StudentsApi
        :<|> AnswersApi
        :<|> LeaderBoardApi
        )

type ClassesApi =
    "classes" :>
        (    Get '[JSON] [Class]
        :<|> Capture "classId" ClassId :>
             (    Get '[JSON] Class
             :<|> Delete '[JSON] NoContent
             :<|> "members" :> ReqBody '[JSON] [ClassId] :> QueryParam "delete" Bool :> Post '[JSON] Class
             )
        :<|> ReqBody '[JSON] (Text, Text) :> Post '[JSON] Class
        )

type StudentsApi =
    "students" :>
        (    Get '[JSON] [Student]
        :<|> Capture "studentId" SubjectId :>
             (    Get '[JSON] Student
             :<|> ReqBody '[JSON] Student :> Post '[JSON] Student
             :<|> "password" :> ReqBody '[JSON] Text :> PostNoContent '[JSON] NoContent
             :<|> "username" :> ReqBody '[JSON] Text :> PostNoContent '[JSON] NoContent
             :<|> Delete '[JSON] Student
             :<|> "undelete" :> PostNoContent '[JSON] NoContent
             )
        :<|> ReqBody '[JSON] [Text] :> Post '[JSON] [(Student, (Text, Text))]
        )

type AnswersApi =
    "answers" :>
        (    QueryParam "story" StoryId :> QueryParam "student" SubjectId :> Get '[JSON] [Answer]
        :<|> ReqBody '[JSON] Answer :> Post '[JSON] Answer
        )

type LeaderBoardApi =
    "leaderboard" :> Get '[JSON] [LeaderBoardEntry]


type TrailsApi =
    "trails" :> AccessTokenAuth :>
        (    Get '[JSON] [StoryTrail]
        :<|> ReqBody '[JSON] StoryTrail :> Post '[JSON] StoryTrail
        )

type Api = StoriesApi :<|> DictApi :<|> SchoolsApi :<|> SchoolApi :<|> TrailsApi :<|> LoginApi :<|> AccountApi
