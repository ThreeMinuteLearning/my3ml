{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Crypto.Random (getRandomBytes)
import           Data.Maybe (fromMaybe)
import           Jose.Jwk
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           System.Environment (getEnvironment)

import           Api.Server (HandlerT, Config(..))
import qualified Api.Server as Api
import           Api.Auth (authServerContext)
import           Api.Types (Api)
import           DB (DB, getStories)
import           HasqlDB (mkDB)

type SiteApi = "api" :> Api
    :<|> Raw

siteApi :: Proxy SiteApi
siteApi = Proxy


server :: forall db. DB db => Config db -> FilePath -> ServerT SiteApi Handler
server config assets = enter transform Api.server :<|> serveDirectory assets
  where
    transform' :: HandlerT db a -> Handler a
    transform' handler =
        runReaderT (runStderrLoggingT handler) config

    transform :: HandlerT db :~> Handler
    transform = Nat transform'


main :: IO ()
main = do
    env <- getEnvironment
    keyBytes <- getRandomBytes 16
    let port = maybe 8000 read $ lookup "PORT" env
        pgdb = fromMaybe "postgresql://threeml:threeml@localhost/my3ml" $ lookup "PGDB" env
        assets = fromMaybe "assets" $ lookup "ASSETS" env
        jwk = SymmetricJwk keyBytes Nothing Nothing Nothing
    putStrLn $ "Serving on port " ++ show port ++ "..."
    db <- mkDB pgdb
    stories <- getStories db
    let cfg = Config db jwk (take 20 stories)
        my3mlServer = server cfg assets

    run port $ logStdoutDev $ serveWithContext siteApi (authServerContext jwk) my3mlServer
