module Data.Settings exposing (Settings, defaultSettings, decoder, encode, toStyle, fontOptions)

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode


type alias Settings =
    { background : String
    , colour : String
    , font : String
    , size : String
    }


defaultSettings : Settings
defaultSettings =
    Settings "#ffffff" "#000000" "" "16px"


toStyle : Settings -> Attribute msg
toStyle { background, colour, size, font } =
    style [ ( "background", background ), ( "color", colour ), ( "font-size", size ), ( "font-family", font ) ]


decoder : Decoder Settings
decoder =
    decode Settings
        |> required "background" Decode.string
        |> required "colour" Decode.string
        |> required "font" Decode.string
        |> required "size" Decode.string


encode : Settings -> Encode.Value
encode s =
    Encode.object
        [ ( "background", Encode.string s.background )
        , ( "colour", Encode.string s.colour )
        , ( "font", Encode.string s.font )
        , ( "size", Encode.string s.size )
        ]


fontOptions : List ( String, String )
fontOptions =
    [ ( "Standard font", "\"Helvetica Neue\",Helvetica,Arial,sans-serif" )
    , ( "Helvetica (Serif)", "\"Helvetica Neue\",Helvetica,Arial,serif" )
    , ( "Comic Sans", "\"Comic Sans\",cursive" )
    ]
