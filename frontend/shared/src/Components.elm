module Components exposing (btn, btnBase, btnSmall, link, panel, toolbar)

import Html exposing (Attribute, Html, button, div, span, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as Svga


panel : List (Attribute msg) -> List (Html msg) -> Html msg
panel attrs =
    div (class "border rounded shadow-md" :: attrs)


link : List (Attribute msg) -> String -> Html msg
link attrs txt =
    Html.a (class "text-blue hover:text-blue-dark no-underline" :: attrs) [ text txt ]


toolbar : List ( msg, Bool, String ) -> List (Html msg) -> Html msg
toolbar buttons elts =
    let
        mkBtn ( msg, disable, txt ) =
            btnSmall
                [ classList [ ( "hover:bg-blue-dark", not disable ), ( "opacity-50 cursor-not-allowed", disable ) ]
                , class "mr-1"
                , onClick msg
                , disabled disable
                , type_ "button"
                ]
                [ text txt ]
    in
    div [ class "flex flex-wrap items-center mt-4 py-1" ]
        (List.map mkBtn buttons ++ elts)


btnBase : List (Attribute msg) -> List (Html msg) -> Html msg
btnBase attrs =
    button (class "text-white font-bold rounded focus:outline-none focus:shadow-outline" :: attrs)


btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn attrs =
    btnBase (class "bg-blue hover:bg-blue-dark py-2 px-3" :: attrs)


btnSmall : List (Attribute msg) -> List (Html msg) -> Html msg
btnSmall attrs =
    btnBase (class "bg-blue hover:bg-blue-dark text-sm px-2 py-1" :: attrs)
