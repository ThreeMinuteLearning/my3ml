module Data.Words exposing (Definition, WordDict)

import Dict exposing (Dict)


type alias WordDict =
    Dict String (List Definition)


type alias Definition =
    ( String, List ( String, Int ) )
