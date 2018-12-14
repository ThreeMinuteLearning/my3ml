module Views.Form exposing (input, password, textarea, viewErrorMsgs, viewErrors)

import Html exposing (Attribute, Html, fieldset, li, text, ul)
import Html.Attributes exposing (class, type_)
import Tuple exposing (second)


password : List (Attribute msg) -> List (Html msg) -> Html msg
password attrs =
    Html.input ([ type_ "password" ] ++ attrs)


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attrs =
    Html.input ([ type_ "text" ] ++ attrs)


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea =
    Html.textarea


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
