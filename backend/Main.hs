{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Catch (catchAll, SomeException)
import           Crypto.Random (getRandomBytes)
import qualified Data.Aeson as A
import           Data.Array.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Jose.Jwa
import           Jose.Jwk
import           Network.Wai.Handler.Warp (run)
import           Prelude hiding (id)
import           Servant
import           System.Environment (getEnvironment)
import           System.Directory (doesFileExist)
import           System.Random (randomRIO)
import qualified Rollbar

import           Api.Server (HandlerT, Config(..))
import qualified Api.Server as Api
import           Api.Auth (authServerContext)
import           Api.Types (Api, Story(..), StoryId)
import           DB (DB, getStories)
import           HasqlDB (mkDB)
import qualified Version

type SiteApi = "api" :> Api
    :<|> Raw

siteApi :: Proxy SiteApi
siteApi = Proxy


server :: forall db. DB db => Config db -> FilePath -> ServerT SiteApi Handler
server config assets = enter transform Api.server :<|> serveDirectoryFileServer assets
  where
    transform' :: HandlerT db a -> Handler a
    transform' handler =
        runReaderT (runStderrLoggingT handler) config `catchAll` errorHandler

    errorHandler e = do
        liftIO $ logE (rollbarSettings config) "3ml" e
        throwError err500

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
        rollbarToken = lookup "ROLLBAR_TOKEN" env
    T.putStrLn $ "3ml server version " <> Version.version
    putStrLn $ "Serving on port " ++ show port ++ "..."
    db <- mkDB pgdb
    starterStories <- getStarterStories db
    let cfg = Config db jwk starterStories (fmap mkRollbarSettings rollbarToken)
        my3mlServer = server cfg assets
        app = serveWithContext siteApi (authServerContext jwk) my3mlServer

    run port app

  where
    mkRollbarSettings token = Rollbar.Settings
        { Rollbar.environment = Rollbar.Environment "production"
        , Rollbar.token = Rollbar.ApiToken (T.pack token)
        , Rollbar.hostName = "3ml"
        , Rollbar.reportErrors = True
        }

    getStarterStories db = do
        stories <- take 100 . reverse . sortOn (id :: Story -> StoryId) <$> getStories db
        take 24 <$> shuffle stories

shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArr xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = length xs
    newArr :: [a] -> IO (IOArray Int a)
    newArr = newListArray (1,n)

logE :: Maybe Rollbar.Settings -> T.Text -> SomeException -> IO ()
logE settings label e =
    case settings of
        Just s -> Rollbar.reportErrorS s opts label message
        _ -> T.putStrLn $ "[Error#" `mappend` label `mappend` "] " `mappend` " " `mappend` message
  where
    message = T.pack (show e)
    -- TODO: Find a way to extract the subjectId from the current request and report it here
    opts = Rollbar.Options
        { Rollbar.person      = Nothing
        , Rollbar.revisionSha = Just Version.revision
        }
