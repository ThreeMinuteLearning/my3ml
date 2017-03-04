module Decode exposing (storiesDecoder, wordDictDecoder)

import Array exposing (Array)
import Date exposing (Date)
import Dict exposing (Dict)
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


dictEntryDecoder : JD.Decoder DictEntry
dictEntryDecoder =
    JD.map2 DictEntry
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
                (JD.field "definitions" (JD.list dictEntryDecoder))
                (JD.field "date" dateDecoder)
                (JD.field "content" JD.string)


wordDictDecoder : JD.Decoder (Dict String (Array Definition))
wordDictDecoder =
    let
        defnArrayDecoder =
            JD.array (defnDecoder)

        dictEntryDecoder =
            JD.map2 DictEntry (JD.index 0 JD.string) (JD.index 1 JD.int)

        defnDecoder =
            JD.list <|
                JD.map2 (,) (JD.index 0 JD.string) (JD.index 1 (JD.list dictEntryDecoder))
    in
        JD.dict (defnArrayDecoder)
