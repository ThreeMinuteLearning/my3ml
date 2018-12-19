module Components exposing (btn, btnBase, link, toolbar)

import Html exposing (Attribute, Html, button, div, span, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as Svga


link : List (Attribute msg) -> String -> Html msg
link attrs txt =
    Html.a (class "text-blue hover:text-blue-dark no-underline" :: attrs) [ text txt ]


toolbar : List ( msg, Bool, String ) -> List (Html msg) -> Html msg
toolbar buttons elts =
    let
        mkBtn ( msg, disable, txt ) =
            btnBase [ class "bg-blue hover:bg-blue-darker text-sm mr-1 mb-1", onClick msg, disabled disable, type_ "button" ] [ text txt ]
    in
    div [ class "flex flex-wrap items-center mt-4 py-1" ]
        (List.map mkBtn buttons ++ [ div [ class "mb-1" ] elts ])


btnBase : List (Attribute msg) -> List (Html msg) -> Html msg
btnBase attrs =
    button (class "text-white font-bold py-2 px-3 rounded focus:outline-none focus:shadow-outline" :: attrs)


btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn attrs =
    btnBase (class "bg-blue hover:bg-blue-dark" :: attrs)
