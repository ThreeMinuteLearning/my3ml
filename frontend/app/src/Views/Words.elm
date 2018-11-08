module Views.Words exposing (view)

import Api exposing (DictEntry)
import Data.Words exposing (WordDict)
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class)


{-| Shows a panel containing one or more dictionary word lookups
-}
view : WordDict -> List DictEntry -> Html msg
view dict words =
    let
        collectDefinitions : DictEntry -> Dict String String -> Dict String String
        collectDefinitions entry defs =
            if Dict.member entry.word defs then
                defs
            else
                Dict.get entry.word dict
                    |> Maybe.andThen (List.head << List.drop entry.index)
                    |> Maybe.map
                        (\( defn, subwords ) ->
                            List.foldl collectDefinitions (Dict.insert entry.word defn defs) (List.map mkEntry subwords)
                        )
                    |> Maybe.withDefault Dict.empty

        render ( w, d ) =
            div [ class "dict-definition" ]
                [ Html.p
                    []
                    [ Html.strong [] [ Html.text w ]
                    , Html.text (": " ++ d)
                    ]
                ]

        mkEntry ( w, i ) =
            DictEntry w i

        originalWords =
            List.map .word words

        uniqueDefinitions =
            Dict.toList (List.foldl collectDefinitions Dict.empty words)

        sortedDefinitions =
            List.partition (\( w, d ) -> List.member w originalWords) uniqueDefinitions
                |> \( defs, subdefs ) -> List.append defs subdefs
    in
        div [] (List.map render sortedDefinitions)
