module Views.Form exposing (input, password, select, textarea, viewErrorMsgs, viewErrors)

import Html exposing (Attribute, Html, fieldset, li, text, ul)
import Html.Attributes exposing (class, type_)
import Tuple exposing (second)


fcStyle : Attribute msg
fcStyle =
    class "border shadow border-grey-light bg-white rounded text-grey-darker focus:border-blue"


fcHeightPadding : Attribute msg
fcHeightPadding =
    class "h-8 px-3 py-1 "


password : List (Attribute msg) -> List (Html msg) -> Html msg
password attrs =
    Html.input ([ fcStyle, fcHeightPadding, type_ "password" ] ++ attrs)


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attrs =
    Html.input ([ fcStyle, fcHeightPadding, type_ "text" ] ++ attrs)


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea attrs =
    Html.textarea (fcStyle :: attrs)


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
    errors
        |> List.map (\error -> li [ class "ml-2 mb-1" ] [ text error ])
        |> ul [ class "list-reset text-red-dark font-bold py-2 mb-1" ]
