module Cache exposing (Cache, clearCache, emptyCache)

import Api
import Data.Words exposing (WordDict)
import Dict exposing (Dict)


type alias Cache =
    { dict : WordDict
    , stories : List Api.Story
    , answers : Dict Int Api.Answer
    , students : List Api.Student
    , classes : List Api.Class
    , anthologies : List Api.Anthology
    , newAccounts : List ( Api.Student, ( String, String ) )
    , selectedStories : List Api.Story
    }


emptyCache : Cache
emptyCache =
    Cache Dict.empty [] Dict.empty [] [] [] [] []


clearCache : Cache -> Cache
clearCache oldCache =
    emptyCache
        |> (\c -> { c | dict = oldCache.dict })
