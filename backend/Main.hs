{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import           Control.Monad.Logger
import           Crypto.Random (getRandomBytes)
import           Data.Maybe (fromMaybe)
import           Jose.Jwk
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
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


server :: DB db => db -> Jwk -> FilePath -> ServerT SiteApi Handler
server db key assets = enter toHandler apiServer :<|> serveDirectory assets
  where
    apiServer = Api.Server.server db key


toHandler' :: forall a. LoggingT Handler a -> Handler a
toHandler' = runStderrLoggingT

toHandler :: LoggingT Handler :~> Handler
toHandler = Nat toHandler'


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
    let my3mlServer = server adb key assets

    run port $ logStdoutDev $ serveWithContext siteApi (authServerContext key) my3mlServer
