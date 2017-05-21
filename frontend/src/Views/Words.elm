module Views.Words exposing (view)

import Api exposing (DictEntry)
import Data.Words exposing (WordDict)
import Dict
import Html exposing (Html, div)
import Html.Attributes exposing (class)


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
            div [ class "dict-definition" ]
                ([ Html.p
                    []
                    [ Html.strong [] [ Html.text w ]
                    , Html.text (": " ++ d)
                    ]
                 ]
                    ++ List.map (viewDefinition << mkEntry) ws
                )

        mkEntry ( w, i ) =
            DictEntry w i
    in
        div []
            (List.map viewDefinition words)