module Views.ClassSelect exposing (view)

import Api
import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


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
        Html.select [ class "form-control", onInput (\s -> onSelect s) ]
            (emptyOption :: List.map classOption classes)
