{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad (join)
import           Data.Monoid ((<>))
import           Data.Proxy  (Proxy (Proxy))
import           Elm (Spec (Spec), specsToDir, toElmTypeSource, toElmDecoderSource, toElmEncoderSource)
import           Servant.Elm (ElmOptions (..), defElmImports, defElmOptions, generateElmForAPIWith, UrlPrefix (Static))

import           Api.Types

elmOpts :: ElmOptions
elmOpts =
    defElmOptions
        { urlPrefix = Static "http://localhost:8000/api" }

specs :: [Spec]
specs =
    [ Spec ["Api"]
        (
            [  "import Dict exposing (Dict)"
            ,  defElmImports
            ]
           <> typeSources
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
        <> sourceFor (Proxy :: Proxy Class)
        <> sourceFor (Proxy :: Proxy Login)
        <> sourceFor (Proxy :: Proxy UserType)
        <> sourceFor (Proxy :: Proxy AccessToken)
        <> sourceFor (Proxy :: Proxy LoginRequest)

    sourceFor t = [ (toElmTypeSource t, [toElmDecoderSource t, toElmEncoderSource t]) ]

main :: IO ()
main = specsToDir specs "frontend/src"
