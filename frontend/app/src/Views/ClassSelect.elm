module Views.ClassSelect exposing (view)

import Api
import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode as Json


view : List Api.Class -> Maybe String -> String -> (String -> msg) -> Html msg
view classes selection name onSelect =
    let
        selectedClass =
            Maybe.withDefault "" selection

        emptyOption =
            Html.option [ value "" ] [ text name ]

        format description =
            description
                |> Maybe.map (\d -> " (" ++ d ++ ")")
                |> Maybe.withDefault ""

        classOption c =
            Html.option
                [ selected (selectedClass == c.id)
                , value c.id
                ]
                [ text (c.name ++ format c.description) ]
    in
        Html.select [ class "form-control", on "change" (Json.map onSelect targetValue) ]
            (emptyOption :: List.map classOption classes)
