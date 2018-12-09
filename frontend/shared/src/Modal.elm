module Modal exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


modalHeader : String -> msg -> Html msg
modalHeader title onClose =
    div [ class "modal-header" ]
        [ button [ type_ "button", class "close", onClick onClose ]
            [ span [] [ text "×" ]
            ]
        , h3 [ class "modal-title" ] [ text title ]
        ]


view : String -> msg -> Html msg -> Html msg
view title onClose content =
    div []
        [ div [ class "modal in", style "display" "block" ]
            [ div [ class "modal-dialog" ]
                [ div [ class "modal-content" ]
                    [ modalHeader title onClose
                    , div [ class "modal-body" ] [ content ]
                    ]
                ]
            ]
        , div [ class "modal-backdrop in" ] []
        ]
