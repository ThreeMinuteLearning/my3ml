{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Prelude hiding (id)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant ((:<|>) ((:<|>)), (:>), Proxy (Proxy), Raw, Server, serveWithContext, serveDirectory)

import qualified Api.Server
import           Api.Auth (authServerContext)
import           Api.Types (Api, Story (..))
import           DB
import qualified TestData

type SiteApi =  "api" :> Api
            :<|> Raw

siteApi :: Proxy SiteApi
siteApi = Proxy

server :: DB db => db -> Server SiteApi
server db = apiServer :<|> assets
  where
    apiServer = Api.Server.server db
    assets = serveDirectory "assets"

main :: IO ()
main = do
  storyFile <- B.readFile "./data/allstories.json"
  dictFile <- B.readFile "./data/dict.json"
  let port = 8000
      stories' = case J.eitherDecodeStrict storyFile of
          Right s -> s
          Left e -> error $ "Failed to decode stories " ++ show e

      dict = case J.eitherDecodeStrict dictFile of
          Right d -> d
          Left e -> error $ "Failed to decode dictionary" ++ show e
      storyIds = map (fromJust . id) stories'
      starterStories' = take 20 stories'
      db = InMemoryDB
          { stories = Map.fromList (zip storyIds stories')
          , sampleStories = starterStories'
          , dictionary = dict
          , trails = []
          , schools = TestData.schools
          , classes = TestData.classes
          , students = TestData.students
          }
  putStrLn $ "Serving on port " ++ show port ++ "..."
  adb <- mkDB db
  run port $ serveWithContext siteApi authServerContext (server adb)
