{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

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
           (defElmImports
            : toElmTypeSource    (Proxy :: Proxy Story)
            : toElmDecoderSource (Proxy :: Proxy Story)
            : toElmEncoderSource (Proxy :: Proxy Story)
            : toElmTypeSource    (Proxy :: Proxy DictEntry)
            : toElmDecoderSource (Proxy :: Proxy DictEntry)
            : toElmEncoderSource (Proxy :: Proxy DictEntry)
            : generateElmForAPIWith elmOpts  (Proxy :: Proxy Api))
    ]

main :: IO ()
main = specsToDir specs "frontend/src"
