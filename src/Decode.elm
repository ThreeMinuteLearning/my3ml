module Decode exposing (storiesDecoder)

import Date exposing (Date)
import Json.Decode as JD
import Types exposing (..)


dateDecoder : JD.Decoder Date
dateDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case Date.fromString str of
                    Err err ->
                        JD.fail err

                    Ok date ->
                        JD.succeed date
            )


definitionDecoder : JD.Decoder Definition
definitionDecoder =
    JD.map2 Definition
        (JD.field "word" JD.string)
        (JD.field "index" JD.int)


storiesDecoder : JD.Decoder (List Story)
storiesDecoder =
    JD.field "stories" <|
        JD.list <|
            JD.map8 Story
                (JD.field "id" JD.string)
                (JD.field "img" JD.string)
                (JD.field "title" JD.string)
                (JD.field "tags" (JD.list JD.string))
                (JD.field "level" JD.int)
                (JD.field "definitions" (JD.list definitionDecoder))
                (JD.field "date" dateDecoder)
                (JD.field "content" JD.string)
