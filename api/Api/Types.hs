{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Types where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Time.Clock (UTCTime)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Elm (ElmType)
import           GHC.Generics (Generic)
import           Prelude hiding (id)
import           Servant ((:<|>), (:>), AuthProtect, Capture, ReqBody, Post, Get, JSON)

data Story = Story
    { id :: Maybe StoryId
    , img :: Text
    , title :: Text
    , tags :: [Text]
    , level :: Int
    , words :: [DictEntry]
    , date :: UTCTime
    , content :: Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data DictEntry = DictEntry
    { word :: Text
    , index :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type StoryId = Text

type WordDefinition = (Text, [(Text, Int)])

type WordDictionary = Map.Map Text [WordDefinition]

data School = School
    { id :: SchoolId
    , name :: Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type SchoolId = Text

data Class = Class
    { id :: ClassId
    , name :: Text
    , schoolId :: SchoolId
    , students :: [StudentId]
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type ClassId = Text

data Teacher = Teacher
    { id :: Text
    , name :: Text
    , schoolId :: SchoolId
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Student = Student
    { id :: StudentId
    , name :: Text
    , level :: Int
    , schoolId :: SchoolId
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type StudentId = Text

data LoginRequest = LoginRequest
    { username :: Text
    , password :: Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Login = Login
    { sub :: SubjectId
    , username :: Text
    , name :: Text
    , role :: UserType
    , token :: AccessToken
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type SubjectId = Text

type AccessToken = Text

-- Change this to an ADT when elm-export support lands
newtype UserType = UserType {userType :: Text }
    deriving (Show, Generic, ElmType, ToJSON, FromJSON)

student, teacher, editor, admin :: UserType
student = UserType "Student"
teacher = UserType "Teacher"
editor = UserType "Editor"
admin = UserType "Admin"

type AccessTokenAuth = AuthProtect "access-token"

type LoginApi =
    "authenticate" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Login

type StoriesApi =
    "stories" :> AccessTokenAuth :>
        (    Get '[JSON] [Story]
        :<|> Capture "storyId" Text :> Get '[JSON] Story
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
             )
        )

type SchoolApi =
    "school" :> AccessTokenAuth :>
         (    ClassesApi
         :<|> StudentsApi
         )

type ClassesApi =
    "classes" :>
        (    Get '[JSON] [Class]
        :<|> Capture "classId" ClassId :> Get '[JSON] Class
        )

type StudentsApi =
    "students" :>
        (    Get '[JSON] [Student]
        :<|> Capture "studentId" StudentId :> Get '[JSON] Student
        )

type Api = StoriesApi :<|> DictApi :<|> SchoolsApi :<|> SchoolApi :<|> LoginApi
