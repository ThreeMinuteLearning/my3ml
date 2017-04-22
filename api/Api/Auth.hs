{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Auth where

import           Data.Aeson
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics (Generic)
import           Jose.Jwe (jwkEncode, jwkDecode)
import           Jose.Jwk (Jwk)
import           Jose.Jwa
import           Jose.Jwt (Jwt(..), JwtContent(..), Payload(Claims))
import           Servant (Context(..), AuthProtect, err400, errBody)
import           Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import           Network.Wai (Request, requestHeaders)

import           Api.Types (SchoolId, SubjectId)

data AccessScope
    = TeacherScope SubjectId SchoolId
    | StudentScope SubjectId SchoolId
    | AdminScope SubjectId
    | EditorScope
      deriving (Generic, FromJSON, ToJSON)

type instance AuthServerData (AuthProtect "access-token") = Maybe AccessScope


authHandler :: Jwk -> AuthHandler Request (Maybe AccessScope)
authHandler k =
  let
    handler req = case lookup "Authorization" (requestHeaders req) of
        Nothing -> return Nothing -- throwError (err401 { errBody = "Missing auth header" })
        Just "" -> return Nothing
        Just header -> do
            t <- liftIO $ jwkDecode k header
            case t of
                Right (Jwe (_, token)) -> maybe badToken (return . Just) (decodeStrict token)
                _ -> badToken

    badToken = throwError (err400 { errBody = "Invalid token" })
  in
    mkAuthHandler handler

authServerContext :: Jwk -> Context (AuthHandler Request (Maybe AccessScope) ': '[])
authServerContext k = authHandler k :. EmptyContext

mkAccessToken :: MonadIO m => Jwk -> AccessScope -> m Text
mkAccessToken k scope = do
    Right (Jwt jwt) <- liftIO $ jwkEncode A128KW A128GCM k (Claims (toStrict (encode scope)))
    return $ decodeUtf8 jwt
