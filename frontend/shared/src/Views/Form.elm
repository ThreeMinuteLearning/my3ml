module Views.Form exposing (input, password, select, textarea, viewErrorMsgs, viewErrors)

import Html exposing (Attribute, Html, fieldset, li, text, ul)
import Html.Attributes exposing (class, type_)
import Tuple exposing (second)


formControl : Attribute msg
formControl =
    class "h-8 border shadow border-grey-light bg-white rounded px-3 py-1 text-grey-darker focus:border-blue"


password : List (Attribute msg) -> List (Html msg) -> Html msg
password attrs =
    Html.input ([ formControl, type_ "password" ] ++ attrs)


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attrs =
    Html.input ([ formControl, type_ "text" ] ++ attrs)


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea attrs =
    Html.textarea (formControl :: attrs)


select : List (Attribute msg) -> List (Html msg) -> Html msg
select attrs =
    Html.select (formControl :: attrs)


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map second
        |> viewErrorMsgs


viewErrorMsgs : List String -> Html msg
viewErrorMsgs errors =
    errors
        |> List.map (\error -> li [] [ text error ])
        |> ul [ class "error-messages" ]
