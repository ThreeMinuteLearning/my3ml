{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Crypto.Random (getRandomBytes)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Jose.Jwa
import           Jose.Jwk
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           System.Environment (getEnvironment)
import           System.Directory (doesFileExist)

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
server config assets = enter transform Api.server :<|> serveDirectoryFileServer assets
  where
    transform' :: HandlerT db a -> Handler a
    transform' handler =
        runReaderT (runStderrLoggingT handler) config

    transform :: HandlerT db :~> Handler
    transform = NT transform'

defaultKeyFile :: FilePath
defaultKeyFile = "token_key.json"

getKey :: FilePath -> IO Jwk
getKey file = do
    exists <- doesFileExist file
    jwks   <- if exists
                  then A.decodeStrict <$> B.readFile file
                  else return Nothing
    case jwks of
        Just (JwkSet (k:_)) -> return k
        _ -> do
            k  <- getRandomBytes 16
            let jwk = SymmetricJwk k Nothing Nothing (Just (Encrypted A128KW))
            BL.writeFile file (A.encode (JwkSet [jwk]))
            return jwk


main :: IO ()
main = do
    env <- getEnvironment
    jwk <- getKey defaultKeyFile
    let port = maybe 8000 read $ lookup "PORT" env
        pgdb = fromMaybe "postgresql://threeml:threeml@localhost/my3ml" $ lookup "PGDB" env
        assets = fromMaybe "assets" $ lookup "ASSETS" env
    putStrLn $ "Serving on port " ++ show port ++ "..."
    db <- mkDB pgdb
    stories <- getStories db
    let cfg = Config db jwk (take 24 stories)
        my3mlServer = server cfg assets

    run port $ logStdoutDev $ serveWithContext siteApi (authServerContext jwk) my3mlServer
