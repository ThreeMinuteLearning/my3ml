port module Ports exposing (..)


port getImgWidth : String -> Cmd msg


port imgWidth : (Float -> msg) -> Sub msg


port printWindow : () -> Cmd msg
