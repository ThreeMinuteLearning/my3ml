port module Ports exposing (dictLookup, getImgWidth, imgWidth, isLastEltVisible, lastEltVisible, postProcessStory, printWindow, scroll, storeSession)

import Api exposing (DictEntry)


port getImgWidth : String -> Cmd msg


port imgWidth : (Float -> msg) -> Sub msg


port printWindow : () -> Cmd msg


port postProcessStory : List DictEntry -> Cmd msg


port dictLookup : (( String, Int ) -> msg) -> Sub msg


port storeSession : Maybe String -> Cmd msg


port isLastEltVisible : String -> Cmd msg


port lastEltVisible : (Bool -> msg) -> Sub msg


port scroll : (Bool -> msg) -> Sub msg
