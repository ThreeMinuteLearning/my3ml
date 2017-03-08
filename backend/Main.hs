{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import           Control.Concurrent.STM (TVar, newTVarIO)
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Prelude hiding (id)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant ((:<|>) ((:<|>)), (:>), Proxy (Proxy), Raw, Server, serve, serveDirectory)

import qualified Api.Server
import           Api.Types (Api, Story (..), DB (..))

type SiteApi =  "api" :> Api
            :<|> Raw

siteApi :: Proxy SiteApi
siteApi = Proxy

server :: TVar DB -> Server SiteApi
server db = apiServer :<|> assets
  where
    apiServer = Api.Server.server db
    assets = serveDirectory "assets"

app :: TVar DB -> Application
app db = serve siteApi (server db)

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
      db = DB
          { stories = Map.fromList (zip storyIds stories')
          , dictionary = dict
          }
  tDB <- newTVarIO db
  putStrLn $ "Serving on port " ++ show port ++ "..."
  run port (app tDB)
