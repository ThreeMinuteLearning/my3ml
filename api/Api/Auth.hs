{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.Auth where

import           Control.Monad.Except (throwError)
import           Data.Text (unpack, pack)
import           Data.Text.Encoding (decodeUtf8')
import           Servant (Context(..), AuthProtect, err400, errBody)
import           Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import           Network.Wai (Request, requestHeaders)

import           Api.Types (SchoolId)

data AccessScope
    = TeacherScope SchoolId
    | StudentScope SchoolId
    | AdminScope
    | EditorScope

type instance AuthServerData (AuthProtect "access-token") = Maybe AccessScope

authHandler :: AuthHandler Request (Maybe AccessScope)
authHandler =
  let
    handler req = case lookup "Authorization" (requestHeaders req) of
        Nothing -> return Nothing -- throwError (err401 { errBody = "Missing auth header" })
        Just header -> case fmap unpack (decodeUtf8' header) of
            Right "" -> return Nothing
            Right "editor" -> return (Just EditorScope)
            Right "admin" -> return (Just AdminScope)
            Right ('t':':':s) -> return . Just . TeacherScope $ pack s
            Right ('s':':':s) -> return . Just . StudentScope $ pack s
            _ -> throwError (err400 {errBody = "Invalid token"})
  in
    mkAuthHandler handler

authServerContext :: Context (AuthHandler Request (Maybe AccessScope) ': '[])
authServerContext = authHandler :. EmptyContext
