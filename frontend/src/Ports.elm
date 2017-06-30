port module Ports exposing (..)

import Api exposing (DictEntry)


port getImgWidth : String -> Cmd msg


port imgWidth : (Float -> msg) -> Sub msg


port printWindow : () -> Cmd msg


port postProcessStory : List DictEntry -> Cmd msg


port dictLookup : (( String, Int ) -> msg) -> Sub msg


port storeSession : Maybe String -> Cmd msg
