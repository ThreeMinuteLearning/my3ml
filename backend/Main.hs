{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import           Network.Wai.Handler.Warp (run)
import           Servant ((:<|>) ((:<|>)), (:>), Proxy (Proxy), Raw, Server, serveWithContext, serveDirectory)

import qualified Api.Server
import           Api.Auth (authServerContext)
import           Api.Types (Api)
import           DB
import           TestData (mkTestDB)

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
  let port = 8000
  putStrLn $ "Serving on port " ++ show port ++ "..."
  adb <- mkTestDB
  run port $ serveWithContext siteApi authServerContext (server adb)
