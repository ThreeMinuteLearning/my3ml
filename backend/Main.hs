{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators        #-}

module Main where

import           Control.Concurrent (newMVar, forkIO)
import           Control.Exception (fromException, SomeException)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Catch (catchAll, catchIf)
import           Crypto.Random (getRandomBytes)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Jose.Jwa
import           Jose.Jwk
import           Network.Wai (Request)
import           Network.Wai.Handler.Warp (run)
import           Prelude hiding (id)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler)
import           System.Environment (getEnvironment)
import           System.IO.Error

import           Api.Auth
import           Api.Server (HandlerT, Config(..))
import qualified Api.Server as Api
import           Api.Auth (authServerContext)
import           Api.Types (Api)
import           DB (DB, getStarterStories)
import           HasqlDB (mkDB, DBException(..))
import qualified Rollbar
import qualified Version

type SiteApi = "api" :> Api
    :<|> Raw


server :: forall db. DB db => Config db -> FilePath -> ServerT SiteApi Handler
server config assets = hoistServerWithContext (Proxy :: Proxy Api) ctx transform Api.server
    :<|> serveDirectoryFileServer assets
  where
    transform :: HandlerT db a -> Handler a
    transform handler =
        runReaderT (runStderrLoggingT handler) config `catchAll` errorHandler

    ctx :: Proxy '[AuthHandler Request (Maybe AccessScope)]
    ctx = Proxy

    errorHandler e = do
        liftIO $ logE (rollbarSettings config) e
        throwError err500


defaultKeyFile :: FilePath
defaultKeyFile = "token_key.json"

defaultRootKeyFile :: FilePath
defaultRootKeyFile = "root_key.json"

getTokenKey :: FilePath -> IO Jwk
getTokenKey file = do
    jwk <- getKey file
    case jwk of
        Just k -> return k
        _ -> do
            k  <- getRandomBytes 16
            let jwk_ = SymmetricJwk k Nothing Nothing (Just (Encrypted A128KW))
            BL.writeFile file (A.encode (JwkSet [jwk_]))
            return jwk_

getKey :: FilePath -> IO (Maybe Jwk)
getKey file = do
    fileContents <- readFileMaybe
    case fileContents of
        Nothing -> return Nothing
        Just bytes -> case A.decodeStrict bytes of
            Just (JwkSet [k]) -> return $ Just k
            Nothing -> error $ "Failed to decode " <> file
            _ -> error $ "File " <> file <> " was expected to contain only one JWK"
  where
    readFileMaybe = catchIf isDoesNotExistError (Just <$> B.readFile file) (const $ return Nothing)

main :: IO ()
main = do
    env <- getEnvironment
    tokenKey_ <- getTokenKey defaultKeyFile
    rootKey_ <- getKey defaultRootKeyFile
    let port = maybe 8000 read $ lookup "PORT" env
        pgdb = fromMaybe "postgresql://threeml:threeml@localhost/my3ml" $ lookup "PGDB" env
        assets = fromMaybe "assets" $ lookup "ASSETS" env
        rollbarToken = lookup "ROLLBAR_TOKEN" env
    T.putStrLn $ "3ml server version " <> Version.version
    T.putStrLn $ "Root key is " <> maybe "unset" (const "set") rootKey_
    putStrLn $ "Serving on port " ++ show port ++ "..."
    db <- mkDB pgdb
    starterStories <- getStarterStories db >>= newMVar
    let cfg = Config db tokenKey_ starterStories (fmap mkRollbarSettings rollbarToken) rootKey_
        my3mlServer = server cfg assets
        app = serveWithContext (Proxy @SiteApi) (authServerContext tokenKey_) my3mlServer

    run port app

  where
    mkRollbarSettings token = Rollbar.Settings
        { Rollbar.environment = "production"
        , Rollbar.token = T.pack token
        , Rollbar.codeVersion = Version.revision
        }

logE :: Maybe Rollbar.Settings -> SomeException -> IO ()
logE settings e = do
    T.putStrLn $ "[Error]" <> " " <> T.pack (show e)
    case settings of
        Just s -> void $ forkIO $ case fromException e of
            Just (DBException msg cs) ->
                Rollbar.sendError s (Rollbar.RollbarException "DBException" (T.pack msg) (Just cs))
            Nothing ->
                Rollbar.sendError s (Rollbar.RollbarException "Unknown" (T.pack $ show e) Nothing)
        _ -> return ()
