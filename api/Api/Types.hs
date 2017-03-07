{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types
    ( Api
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
import           Servant ((:<|>), (:>), ReqBody, Post, Get, JSON)

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
    } deriving (Eq, Ord, Show, Generic, ElmType, ToJSON, FromJSON)

type StoryId = Text

data DB = DB
    { stories :: Map.Map StoryId Story
    }

type Api =
    "stories" :> ( Get '[JSON] [Story]
              :<|> ReqBody '[JSON] Story :> Post '[JSON] Story
                 )
