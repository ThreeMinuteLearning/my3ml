module Data.Settings exposing (Settings, defaultSettings, decoder, encode, toStyle)

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode


type alias Settings =
    { background : String
    , colour : String
    , size : String
    }


defaultSettings : Settings
defaultSettings =
    Settings "#ffffff" "#000000" "16px"


toStyle : Settings -> Attribute msg
toStyle { background, colour, size } =
    style [ ( "background", background ), ( "color", colour ) ]


decoder : Decoder Settings
decoder =
    decode Settings
        |> required "background" Decode.string
        |> required "colour" Decode.string
        |> required "size" Decode.string


encode : Settings -> Encode.Value
encode s =
    Encode.object
        [ ( "background", Encode.string s.background )
        , ( "colour", Encode.string s.colour )
        , ( "size", Encode.string s.size )
        ]
