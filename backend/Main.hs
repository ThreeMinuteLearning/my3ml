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
import           Data.Array.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.List (sortOn)
import           Jose.Jwa
import           Jose.Jwk
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           System.Environment (getEnvironment)
import           System.Directory (doesFileExist)
import           System.Random (randomRIO)

import           Api.Server (HandlerT, Config(..))
import qualified Api.Server as Api
import           Api.Auth (authServerContext)
import           Api.Types (Api, Story(..), StoryId)
import           DB (DB, getStories)
import           HasqlDB (mkDB)
import           Prelude hiding (id)

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
    starterStories <- getStarterStories db
    let cfg = Config db jwk starterStories
        my3mlServer = server cfg assets

    run port $ logStdoutDev $ serveWithContext siteApi (authServerContext jwk) my3mlServer

  where
    getStarterStories db = do
        stories <- take 100 . reverse . sortOn (id :: Story -> StoryId) <$> getStories db
        take 24 <$> shuffle stories

shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
