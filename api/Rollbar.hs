{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Rollbar where

import           Control.Exception
import           Control.Monad (void)
import           Data.Aeson
import           GHC.Generics
import           GHC.Stack.Types (CallStack, getCallStack, SrcLoc(..))
import           Data.Text (Text)
import qualified Data.Vector as V
import           Network.HTTP.Conduit (RequestBody(RequestBodyLBS), Request(method, requestBody), parseUrlThrow, newManager, tlsManagerSettings, httpLbs )

default (Text)

data Settings = Settings
    { environment :: Text
    , token :: Text
    , codeVersion :: Text
    } deriving Show

data Person = Person
    { id :: Text
    , username :: Maybe Text
    , email :: Maybe Text
    } deriving (Generic, ToJSON)

data RollbarException = RollbarException
    { className :: Text
    , message :: Text
    , trace :: Maybe CallStack
    }

sendError
    :: Settings
    -> RollbarException
    -> IO ()
sendError settings RollbarException {..} =
    sendRequest `catch` (\(e :: SomeException) -> print e)
  where
    sendRequest = do
      initReq <- parseUrlThrow "https://api.rollbar.com/api/1/item/"
      mgr <- newManager tlsManagerSettings
      let req = initReq { method = "POST", requestBody = RequestBodyLBS (encode rollbarJson) }
      void $ httpLbs req mgr

    frames = reverse $ case trace of
        Nothing -> []
        Just cs -> map mkFrame (getCallStack cs)

    mkFrame (fnName, SrcLoc{..}) =
        object ["method" .= fnName, "filename" .= srcLocFile, "lineno" .= srcLocStartLine, "colno" .= srcLocStartCol ]

    rollbarJson = object
        [ "access_token" .= token settings
        , "data" .= object
            [ "environment" .= environment settings
            , "body"        .= object
                [ "trace" .= object
                    [ "frames" .= Array (V.fromList frames)
                    , "exception" .= object ["class" .= className, "message" .= message]
                    ]
                ]
            , "code_version" .= codeVersion settings
            ]
        ]
