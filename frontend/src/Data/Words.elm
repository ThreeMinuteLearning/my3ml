module Data.Words exposing (WordDict, Definition)

import Dict exposing (Dict)


type alias WordDict =
    Dict String (List Definition)


type alias Definition =
    ( String, List ( String, Int ) )
