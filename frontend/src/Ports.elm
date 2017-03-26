port module Ports exposing (getImgWidth, imgWidth)


port getImgWidth : String -> Cmd msg


port imgWidth : (Float -> msg) -> Sub msg
