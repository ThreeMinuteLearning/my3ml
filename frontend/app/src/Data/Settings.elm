module Data.Settings exposing (Settings, decoder, defaultSettings, encode, fontOptions, toStyle)

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Settings =
    { background : String
    , colour : String
    , font : String
    , size : String
    , workQueue : List Int
    }


defaultSettings : Settings
defaultSettings =
    Settings "#ffffff" "#000000" "\"Helvetica Neue\",Helvetica,Arial,sans-serif" "1.25rem" []


toStyle : Settings -> List (Attribute msg)
toStyle { background, colour, size, font } =
    [ style "background" background
    , style "color" colour
    , style "font-size" size
    , style "font-family" font
    ]


decoder : Decoder Settings
decoder =
    Decode.succeed Settings
        |> required "background" Decode.string
        |> required "colour" Decode.string
        |> required "font" Decode.string
        |> required "size" Decode.string
        |> optional "workQueue" (Decode.list Decode.int) []


encode : Settings -> Encode.Value
encode s =
    let
        displaySettings =
            [ ( "background", Encode.string s.background )
            , ( "colour", Encode.string s.colour )
            , ( "font", Encode.string s.font )
            , ( "size", Encode.string s.size )
            ]
    in
    Encode.object
        (if List.isEmpty s.workQueue then
            displaySettings

         else
            ( "workQueue", Encode.list Encode.int s.workQueue ) :: displaySettings
        )


fontOptions : List ( String, String )
fontOptions =
    [ ( "Standard font", "\"Helvetica Neue\",Helvetica,Arial,sans-serif" )
    , ( "Helvetica (Serif)", "\"Helvetica Neue\",Helvetica,Arial,serif" )
    , ( "Comic Sans", "\"Comic Sans\",cursive" )
    ]
