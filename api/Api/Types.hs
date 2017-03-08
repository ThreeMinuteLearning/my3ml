{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types
    ( Api
    , StoriesApi
    , DictApi
    , Story (..)
    , DictEntry (..)
    , DB (..)
    , StoryId
    ) where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Elm (ElmType)
import           GHC.Generics (Generic)
import           Prelude hiding (id)
import           Servant ((:<|>), (:>), Capture, ReqBody, Post, Get, JSON)

data Story = Story
    { id :: Maybe StoryId
    , img :: Text
    , title :: Text
    , tags :: [Text]
    , level :: Int
    , words :: [DictEntry]
    , date :: Text
    , content :: Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data DictEntry = DictEntry
    { word :: Text
    , index :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

type StoryId = Text


type WordDefinition = (Text, [(Text, Int)])

type WordDictionary = Map.Map Text [WordDefinition]

data DB = DB
    { stories :: Map.Map StoryId Story
    , dictionary :: WordDictionary
    }

type StoriesApi =
    "stories" :>
        (    Get '[JSON] [Story]
        :<|> Capture "storyId" Text :> Get '[JSON] Story
        :<|> ReqBody '[JSON] Story :> Post '[JSON] Story
        )

type DictApi =
    "dictionary" :>
        (    Get '[JSON] WordDictionary
        :<|> Capture "word" Text :> Get '[JSON] [WordDefinition]
        )

type Api = StoriesApi :<|> DictApi
