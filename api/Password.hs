{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Password
    ( hashPassword
    , validatePassword
    , validatePasswordEither
    , Options(..)
    , defaultOptions
    )
where

import           Crypto.Error
import           Crypto.KDF.Argon2
import           Crypto.Random (MonadRandom, getRandomBytes)
import           Control.Error
import           Data.List (find)
import           Data.ByteArray.Encoding
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Text.Read (readMaybe)

data Argon2Hash = A2H Options ByteString ByteString

-- | Hashes a password using the given options using 16 bytes of salt, producing a
-- 32 byte hash
hashPassword :: MonadRandom m
    => Options
    -> ByteString
    -> m ByteString
hashPassword options password = do
    salt <- getRandomBytes 16
    let CryptoPassed hashed = hash options password (salt :: ByteString) 32
    return $ B.concat [formatOptions options, "$", b64 salt, "$", b64 hashed]
  where
    b64 = fst . B.spanEnd ('=' ==) . convertToBase Base64

formatOptions :: Options -> ByteString
formatOptions Options{..} =
    B.concat ["$", var, "$", ver, "$m=", w2bs memory, ",t=", w2bs iterations, ",p=", w2bs parallelism ]
  where
    var = case variant of
        Argon2i -> "argon2i"
        Argon2d -> "argon2d"
        Argon2id -> "argon2id"
    ver = case version of
        Version13 -> "v=19"
        Version10 -> "v=16"
    w2bs = B.pack . show

-- | Check a password against a stored argon2 hash when authenticating a user.
--
-- Returns @False@ if the password doesn't match the hash, or if the hash is invalid
validatePassword :: ByteString -> ByteString -> Bool
validatePassword password bcHash = either (const False) id (validatePasswordEither password bcHash)

validatePasswordEither :: ByteString -> ByteString -> Either String Bool
validatePasswordEither password a2Hash = do
    A2H options salt hashBytes <- parseArgon2Hash a2Hash
    let CryptoPassed passHash = hash options password salt (B.length hashBytes) :: CryptoFailable ByteString
    return $ passHash `BA.constEq` hashBytes

-- $argon2i$v=19$m=65536,t=2,p=4$c29tZXNhbHQ$RdescudvJCsgt3ub+b+dWRWJTmaaJObG
parseArgon2Hash :: ByteString -> Either String Argon2Hash
parseArgon2Hash a2 = case B.split '$' a2 of
    ["", variantStr, versionStr, optionsStr, saltStr, hashStr] -> do
        var <- parseVariant variantStr
        ver <- parseVersion versionStr
        (m, t, p) <- parseOptions optionsStr
        saltBytes <- b64Decode saltStr
        hashBytes <- b64Decode hashStr
        let
            options = Options
                { iterations = t
                , memory = m
                , parallelism = p
                , variant = var
                , version = ver
                }
        return (A2H options saltBytes hashBytes)
    _ -> Left "Invalid argon2 string"

b64Decode :: ByteString -> Either String ByteString
b64Decode bs = convertFromBase Base64 bsPadded
  where
    bsPadded = case B.length bs `mod` 4 of
        3 -> bs `B.snoc` '='
        2 -> bs `B.append` "=="
        _ -> bs

parseVariant :: ByteString -> Either String Variant
parseVariant v = case v of
    "argon2i" -> pure Argon2i
    "argon2d" -> pure Argon2d
    "argon2id" -> pure Argon2id
    _ -> Left ("Invalid argon2 variant: " ++ B.unpack v)

parseVersion :: ByteString -> Either String Version
parseVersion v = case v of
    "v=19" -> pure Version13
    "v=16" -> pure Version10
    _ -> Left ("Invalid argon2 version: " ++ B.unpack v)

parseOptions :: ByteString -> Either String (MemoryCost, TimeCost, Parallelism)
parseOptions s = case map (B.break (== '=')) (B.split ',' s) of
    os@[_, _, _] -> do
        m <- getOption "m" os
        t <- getOption "t" os
        p <- getOption "p" os
        pure (m, t, p)
    _ -> Left ("Invalid argon2 options: " ++ B.unpack s)
  where
    getOption name os = note ("Failed to parse option: " ++ B.unpack name)
            $ find ((==) name . fst) os >>= readMaybe . B.unpack . B.dropWhile (== '=') . snd
