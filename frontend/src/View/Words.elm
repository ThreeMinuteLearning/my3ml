module View.Words exposing (view)

import Api exposing (DictEntry)
import Dict
import Html exposing (Html, div)
import Types exposing (WordDict)


{-| Shows a panel containing one or more dictionary word lookups
-}
view : WordDict -> List DictEntry -> Html msg
view dict words =
    let
        viewDefinition entry =
            Dict.get entry.word dict
                |> Maybe.andThen (List.head << List.drop entry.index)
                |> Maybe.andThen (Just << render entry.word)
                |> Maybe.withDefault (Html.text "")

        render w ( d, ws ) =
            Html.p []
                [ Html.strong [] [ Html.text w ]
                , Html.text (": " ++ d)
                ]
    in
        div []
            (List.map viewDefinition words)
