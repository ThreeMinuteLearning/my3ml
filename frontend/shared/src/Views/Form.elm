module Views.Form exposing (checkbox, input, label, password, select, textarea, viewErrorMsgs, viewErrors)

import Html exposing (Attribute, Html, fieldset, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Tuple exposing (second)


fcStyle : Attribute msg
fcStyle =
    class "border shadow border-gray-light bg-white rounded text-gray-800 focus:border-blue"


fcHeightPadding : Attribute msg
fcHeightPadding =
    class "h-8 px-3 py-1 "


password : List (Attribute msg) -> List (Html msg) -> Html msg
password attrs =
    Html.input ([ fcStyle, fcHeightPadding, type_ "password" ] ++ attrs)


checkbox : msg -> Bool -> String -> Html msg
checkbox msg checked_ lbl =
    label [ class "flex items-center" ]
        [ text lbl
        , input [ class "mx-2", type_ "checkbox", checked checked_, onClick msg ] []
        ]


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attrs =
    Html.input ([ fcStyle, fcHeightPadding, type_ "text" ] ++ attrs)


label : List (Attribute msg) -> List (Html msg) -> Html msg
label attrs =
    Html.label (class "block text-gray-800 text-sm font-bold" :: attrs)


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea attrs =
    Html.textarea (fcStyle :: class "p-2" :: attrs)


select : List (Attribute msg) -> List (Html msg) -> Html msg
select attrs =
    Html.select (fcStyle :: attrs)


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map second
        |> viewErrorMsgs


viewErrorMsgs : List String -> Html msg
viewErrorMsgs errors =
    if List.isEmpty errors then
        text ""

    else
        errors
            |> List.map (\error -> li [ class "ml-2 mb-1" ] [ text error ])
            |> ul [ id "error-messages", class "text-red-600 font-bold py-2 mb-1" ]
