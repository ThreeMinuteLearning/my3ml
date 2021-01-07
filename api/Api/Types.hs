{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Types where

import           Data.Aeson
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Map.Strict as Map
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Jose.Jwk (Jwk)
import           GHC.Generics (Generic)
import           Prelude hiding (id)
import           Servant ((:<|>), (:>), AuthProtect, Capture, QueryParam, ReqBody, Delete, Post, PostNoContent, NoContent, Get, JSON)
import           Web.HttpApiData

data Story = Story
    { id :: StoryId
    , title :: Text
    , img :: Text
    , level :: Int
    , qualification :: Maybe Text
    , curriculum :: Maybe Text
    , tags :: [Text]
    , content :: Text
    , words :: [DictEntry]
    , clarifyWord :: Text
    , enabled :: Bool
    , createdAt :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

data StoryData = StoryData
    { stories :: [Story]
    , graph :: StoryGraph
    } deriving (Generic, ToJSON)

data GraphEdge = GraphEdge
    { from :: StoryId
    , to :: StoryId
    , description :: Text
    } deriving (Generic, ToJSON)

type StoryGraph = [GraphEdge]

data DictEntry = DictEntry
    { word :: Text
    , index :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

type StoryId = Int

type WordDefinition = (Text, [(Text, Int)])

type WordDictionary = Map.Map Text [WordDefinition]

data School = School
    { id :: SchoolId
    , name :: Text
    , description :: Maybe Text
    } deriving (Show, Generic, ToJSON)

type SchoolId = Text

data Class = Class
    { id :: ClassId
    , name :: Text
    , description :: Maybe Text
    , schoolId :: SchoolId
    , createdBy :: SubjectId
    , students :: [SubjectId]
    } deriving (Show, Generic, ToJSON, FromJSON)

type ClassId = Text

data Answer = Answer
    { storyId :: StoryId
    , studentId :: SubjectId
    , connect :: Text
    , question :: Text
    , summarise :: Text
    , clarify :: Text
    , createdAt :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

data Teacher = Teacher
    { id :: SubjectId
    , name :: Text
    , bio :: Maybe Text
    , schoolId :: SchoolId
    } deriving (Show, Generic, ToJSON, FromJSON)

data Student = Student
    { id :: SubjectId
    , name :: Text
    , description :: Maybe Text
    , level :: Int
    , schoolId :: SchoolId
    , hidden :: Bool
    , deleted :: Maybe POSIXTime
    , createdAt :: POSIXTime
    } deriving (Show, Generic, ToJSON, FromJSON)

data LoginRequest = LoginRequest
    { username :: Text
    , password :: Text
    , otp :: Maybe Int
    , ua :: String
    } deriving (Show, Generic, FromJSON)

data Anthology = Anthology
    { id :: AnthologyId
    , name :: Text
    , description :: Text
    , createdBy :: SubjectId
    , schoolId :: Maybe SchoolId
    , stories :: [StoryId]
    , hidden :: Bool
    } deriving (Show, Generic, ToJSON, FromJSON)

type AnthologyId = Text

data Account = Account
    { id :: SubjectId
    , username :: Text
    , password :: Text
    , otpKey :: Maybe ByteString
    , role :: UserType
    , level :: Int
    , active :: Bool
    , lastLogin :: Maybe POSIXTime
    , settings :: Maybe Value
    }

data UserKeys = UserKeys
    { salt :: ByteString
    , pubKey :: Jwk
    , privKey :: Text
    , schoolKey :: Maybe ByteString
    }

data Login = Login
    { sub :: SubjectId
    , username :: Text
    , name :: Text
    , role :: UserType
    , level :: Int
    , settings :: Maybe Value
    , token :: AccessToken
    } deriving (Show, Generic, ToJSON)

data LeaderBoardEntry = LeaderBoardEntry
    { position :: Int
    , name :: Text
    , studentId :: SubjectId
    , score :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

data Registration = Registration
    { email :: Text
    , code :: Maybe Text
    , schoolName :: Text
    , teacherName :: Text
    , password :: Text
    } deriving (Show, Generic, FromJSON)

newtype SubjectId = SubjectId { unSubjectId :: Text} deriving (Show, Eq, Generic)

instance FromHttpApiData SubjectId where
    parseUrlPiece = Right . SubjectId

instance ToJSON SubjectId where
    toJSON = String . unSubjectId

instance FromJSON SubjectId where
    parseJSON = withText "SubjectId" (pure . SubjectId)

type AccessToken = Text

-- Change this to an ADT when elm-export support lands
newtype UserType = UserType {userType :: Text }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

student, teacher, schoolAdmin, editor, admin :: UserType
student = UserType "Student"
teacher = UserType "Teacher"
schoolAdmin = UserType "SchoolAdmin"
editor = UserType "Editor"
admin = UserType "Admin"

type AccessTokenAuth = AuthProtect "access-token"

type LoginApi =
    "authenticate" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Login

type AdminApi =
    "admin" :> AccessTokenAuth :>
        ( "stats" :> Get '[JSON] Value
        )

type AccountApi =
    "account" :> AccessTokenAuth :>
        (    "settings" :> ReqBody '[JSON] Value :> Post '[JSON] NoContent
        :<|> "register" :> ReqBody '[JSON] Registration :> Post '[JSON] NoContent
        :<|> "register" :> "code" :> Get '[JSON] Text
        )

type StoriesApi =
    "stories" :> AccessTokenAuth :>
        (    Get '[JSON] StoryData
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
             :<|> TeachersApi
             )
        )

type SchoolApi =
    "school" :> AccessTokenAuth :>
        (    ClassesApi
        :<|> StudentsApi
        :<|> AnswersApi
        :<|> LeaderBoardApi
        :<|> TeachersApi
        )

type ClassesApi =
    "classes" :>
        (    Get '[JSON] [Class]
        :<|> Capture "classId" ClassId :>
             (    Get '[JSON] Class
             :<|> Delete '[JSON] NoContent
             :<|> "members" :> ReqBody '[JSON] [SubjectId] :> QueryParam "delete" Bool :> Post '[JSON] Class
             )
        :<|> ReqBody '[JSON] (Text, Text) :> Post '[JSON] Class
        )

type StudentsApi =
    "students" :>
        (    Get '[JSON] [Student]
        :<|> Capture "studentId" SubjectId :>
             (    Get '[JSON] Student
             :<|> ReqBody '[JSON] Student :> Post '[JSON] Student
             :<|> "password" :> ReqBody '[JSON] Text :> PostNoContent
             :<|> "username" :> ReqBody '[JSON] Text :> PostNoContent
             :<|> Delete '[JSON] Student
             :<|> "undelete" :> PostNoContent
             )
        :<|> ReqBody '[JSON] (Int, [Text]) :> Post '[JSON] [(Student, (Text, Text))]
        )


type TeachersApi =
    "teachers" :>
        (    Get '[JSON] [(Teacher, Bool)]
        :<|> Capture "teacherId" SubjectId :>
            (
             "activate" :> Post '[JSON] SubjectId
            )
        )

type AnswersApi =
    "answers" :>
        (    QueryParam "story" StoryId :> QueryParam "student" SubjectId :> Get '[JSON] [Answer]
        :<|> ReqBody '[JSON] Answer :> Post '[JSON] Answer
        )

type LeaderBoardApi =
    "leaderboard" :> Get '[JSON] [LeaderBoardEntry]


type AnthologiesApi =
    "anthologies" :> AccessTokenAuth :>
        (    Get '[JSON] [Anthology]
        :<|> ReqBody '[JSON] Anthology :> Post '[JSON] Anthology
        :<|> Capture "anthologyId" AnthologyId :>
            (    "starter_stories" :> PostNoContent
            :<|> ReqBody '[JSON] Anthology :> Post '[JSON] Anthology
            :<|> Delete '[JSON] AnthologyId
            )
        )

type Api = StoriesApi :<|> DictApi :<|> SchoolsApi :<|> SchoolApi :<|> AnthologiesApi :<|> LoginApi :<|> AccountApi :<|> AdminApi
