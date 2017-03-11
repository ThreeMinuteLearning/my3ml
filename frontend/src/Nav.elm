module Nav exposing (navbar)

import Html exposing (Html, div, a, span, ul, li, nav, button, text, header)
import Html.Attributes exposing (class, id, href, attribute, type_)


navbar : List (Html msg) -> Html msg
navbar links =
    header []
        [ nav [ class "navbar navbar-default" ]
            [ div [ class "container" ]
                [ div [ class "navbar-header" ]
                    [ mobileToggleButton
                    , navbrand "#" "3ML"
                    ]
                , div [ id "navbar", class "navbar-collapse collapse" ]
                    [ ul [ class "nav navbar-nav navbar-right" ] links
                    ]
                ]
            ]
        ]


navbrand : String -> String -> Html msg
navbrand url brandName =
    a [ class "navbar-brand", href url ] [ text brandName ]


mobileToggleButton : Html msg
mobileToggleButton =
    button [ type_ "button", class "navbar-toggle collapsed", attribute "data-toggle" "collapse", attribute "data-target" "#navbar", attribute "aria-expanded" "false" ]
        [ span [ class "sr-only" ] [ text "Toggle Navigation" ]
        , span [ class "icon-bar" ] []
        , span [ class "icon-bar" ] []
        , span [ class "icon-bar" ] []
        ]
