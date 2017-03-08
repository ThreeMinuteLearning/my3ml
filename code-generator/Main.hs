{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

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
            <> sourceFor (Proxy :: Proxy Story)
            <> sourceFor (Proxy :: Proxy DictEntry)
            <> sourceFor (Proxy :: Proxy School)
            <> sourceFor (Proxy :: Proxy Class)
            <> generateElmForAPIWith elmOpts (Proxy :: Proxy Api)
        )
    ]
  where
    sourceFor t = [ toElmTypeSource t, toElmDecoderSource t, toElmEncoderSource t ]

main :: IO ()
main = specsToDir specs "frontend/src"
