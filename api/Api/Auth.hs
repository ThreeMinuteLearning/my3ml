{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.Auth where

import           Control.Monad.Except (throwError)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')
import           Servant (Context(..), AuthProtect, err400, errBody)
import           Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import           Network.Wai (Request, requestHeaders)


type instance AuthServerData (AuthProtect "access-token") = Maybe Text

authHandler :: AuthHandler Request (Maybe Text)
authHandler =
  let
    handler req = case lookup "Authorization" (requestHeaders req) of
        Nothing -> return Nothing -- throwError (err401 { errBody = "Missing auth header" })
        Just header -> case decodeUtf8' header of
            Left _ -> throwError (err400 {errBody = "Invalid token"})
            Right "" -> return Nothing
            Right t -> return (Just t)
  in
    mkAuthHandler handler

authServerContext :: Context (AuthHandler Request (Maybe Text) ': '[])
authServerContext = authHandler :. EmptyContext
