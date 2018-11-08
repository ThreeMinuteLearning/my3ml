port module Ports exposing (..)

import Api exposing (DictEntry)
import Json.Encode as Json


port getImgWidth : String -> Cmd msg


port imgWidth : (Float -> msg) -> Sub msg


port printWindow : () -> Cmd msg


port postProcessStory : List DictEntry -> Cmd msg


port dictLookup : (( String, Int ) -> msg) -> Sub msg


port storeSession : Maybe String -> Cmd msg


port checkPassword : String -> Cmd msg


port passwordChecked : (Json.Value -> msg) -> Sub msg


port isLastEltVisible : String -> Cmd msg


port lastEltVisible : (Bool -> msg) -> Sub msg


port scroll : (Bool -> msg) -> Sub msg
