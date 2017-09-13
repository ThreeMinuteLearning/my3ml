{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Control.Monad (join)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy (Proxy))
import           Data.Text (Text)
import           Elm
import           GHC.TypeLits (KnownSymbol)
import           Servant.Elm (ElmOptions (..), defElmImports, defElmOptions, generateElmForAPIWith, UrlPrefix (Static))
import           Servant.Foreign hiding (Static)

import           Api.Types

elmOpts :: ElmOptions
elmOpts =
    defElmOptions
        { urlPrefix = Static "/api" }

specs :: [Spec]
specs =
    [ Spec ["Api"]
        (
            [  "import Date exposing (Date)"
            ,  "import Dict exposing (Dict)"
            ,  "import Exts.Date exposing (toISOString)"
            ,  "import Exts.Json.Encode exposing (tuple2)"
            ,  "import Exts.Json.Decode exposing (decodeDate)"
            ,  defElmImports
            ]
           <> typeSources
           <> [ toElmTypeSource (Proxy :: Proxy NoContent)]
           <> generateElmForAPIWith elmOpts (Proxy :: Proxy Api)
           <> codecSources
        )
    ]
  where
    typeSources = map fst sources
    codecSources = join (map snd sources)
    sources =
        sourceFor (Proxy :: Proxy Story)
        <> sourceFor (Proxy :: Proxy DictEntry)
        <> sourceFor (Proxy :: Proxy School)
        <> sourceFor (Proxy :: Proxy Answer)
        <> sourceFor (Proxy :: Proxy Class)
        <> sourceFor (Proxy :: Proxy Login)
        <> sourceFor (Proxy :: Proxy UserType)
        <> sourceFor (Proxy :: Proxy Student)
        <> sourceFor (Proxy :: Proxy Teacher)
        <> sourceFor (Proxy :: Proxy StoryTrail)
        <> sourceFor (Proxy :: Proxy LoginRequest)
        <> sourceFor (Proxy :: Proxy LeaderBoardEntry)
        <> sourceFor (Proxy :: Proxy Registration)
--        <> sourceFor (Proxy :: Proxy SubjectId)

    sourceFor t = [ (toElmTypeSource t, [toElmDecoderSource t, toElmEncoderSource t]) ]

-- Add Authorization header argument to APIs with AuthProtect in them
instance (KnownSymbol sym, HasForeignType lang ftype Text, HasForeign lang ftype sublayout)
    => HasForeign lang ftype (AuthProtect sym :> sublayout) where
    type Foreign ftype (AuthProtect sym :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy sublayout) req'
      where
        req' = req { _reqHeaders = HeaderArg arg : _reqHeaders req }
        arg = Arg
            { _argName = PathSegment "Authorization"
            , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
            }

deriving instance ElmType Story
deriving instance ElmType DictEntry
deriving instance ElmType School
deriving instance ElmType Answer
deriving instance ElmType Class
deriving instance ElmType Login
deriving instance ElmType UserType
deriving instance ElmType Student
deriving instance ElmType Teacher
deriving instance ElmType StoryTrail
deriving instance ElmType LoginRequest
deriving instance ElmType LeaderBoardEntry
deriving instance ElmType Registration

instance ElmType SubjectId where
    toElmType _ = toElmType (Proxy :: Proxy Text)

main :: IO ()
main = specsToDir specs "frontend/src"
