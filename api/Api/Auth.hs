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
import           Data.ByteArray as BA
import           Data.ByteArray.Encoding (convertFromBase, convertToBase, Base(Base64URLUnpadded))
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           GHC.Generics (Generic)
import           Jose.Jwe (jwkEncode, jwkDecode)
import           Jose.Jwk (Jwk)
import           Jose.Jwa
import           Jose.Jwt (Jwt(..), JwtContent(..), Payload(Claims))
import           Servant (Context(..), AuthProtect, err401, errBody)
import           Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import           Network.Wai (Request, requestHeaders)

import           Api.Types (SchoolId, SubjectId)


newtype TenantKey = TenantKey ScrubbedBytes

instance FromJSON TenantKey where
    parseJSON = withText "TenantKey" $ \t ->
        case convertFromBase Base64URLUnpadded (encodeUtf8 t) of
            Left  _  -> fail "could not base64 decode tenant key"
            Right b  -> pure $ TenantKey b

instance ToJSON TenantKey where
    toJSON (TenantKey bytes) = String (decodeUtf8 (convertToBase Base64URLUnpadded bytes))

data AccessScope
    = TeacherScope SubjectId SchoolId TenantKey Bool
    | StudentScope SubjectId SchoolId
    | AdminScope SubjectId
    | EditorScope SubjectId
      deriving (Generic, FromJSON, ToJSON)

type instance AuthServerData (AuthProtect "access-token") = Maybe AccessScope


scopeSubjectId :: AccessScope -> SubjectId
scopeSubjectId (TeacherScope s _ _ _) = s
scopeSubjectId (StudentScope s _) = s
scopeSubjectId (AdminScope s) = s
scopeSubjectId (EditorScope s) = s

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

    badToken = throwError (err401 { errBody = "Invalid token" })
  in
    mkAuthHandler handler

authServerContext :: Jwk -> Context (AuthHandler Request (Maybe AccessScope) ': '[])
authServerContext k = authHandler k :. EmptyContext

mkAccessToken :: MonadIO m => Jwk -> AccessScope -> m Text
mkAccessToken jwk scope = do
    encoded <- liftIO $ jwkEncode A128KW A128GCM jwk (Claims (toStrict (encode scope)))
    case encoded of
        Right (Jwt jwt) -> return $ decodeUtf8 jwt
        _ -> error "Failed to decode access token"
