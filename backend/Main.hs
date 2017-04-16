{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import           Crypto.Random (getRandomBytes)
import           Data.Maybe (fromMaybe)
import           Jose.Jwk
import           Network.Wai.Handler.Warp (run)
import           Servant ((:<|>) ((:<|>)), (:>), Proxy (Proxy), Raw, Server, serveWithContext, serveDirectory)
import           System.Environment (getEnvironment)

import qualified Api.Server
import           Api.Auth (authServerContext)
import           Api.Types (Api)
import           DB (DB)
import           HasqlDB (mkDB)

type SiteApi = "api" :> Api
    :<|> Raw

siteApi :: Proxy SiteApi
siteApi = Proxy

server :: DB db => db -> Jwk -> FilePath -> Server SiteApi
server db key assets = apiServer :<|> serveDirectory assets
  where
    apiServer = Api.Server.server db key

main :: IO ()
main = do
    env <- getEnvironment
    keyBytes <- getRandomBytes 16
    let port = maybe 8000 read $ lookup "PORT" env
        pgdb = fromMaybe "postgresql://threeml:threeml@localhost/my3ml" $ lookup "PGDB" env
        assets = fromMaybe "assets" $ lookup "ASSETS" env
        key = SymmetricJwk keyBytes Nothing Nothing Nothing
    putStrLn $ "Serving on port " ++ show port ++ "..."
    adb <- mkDB pgdb

    run port $ serveWithContext siteApi (authServerContext key) (server adb key assets)
