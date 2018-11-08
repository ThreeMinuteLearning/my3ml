module Views.Form exposing (viewErrors, viewErrorMsgs, input, textarea, password)

import Html exposing (fieldset, ul, li, Html, Attribute, text)
import Html.Attributes exposing (class, type_)
import Tuple exposing (second)


password : List (Attribute msg) -> List (Html msg) -> Html msg
password attrs =
    control Html.input ([ type_ "password" ] ++ attrs)


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attrs =
    control Html.input ([ type_ "text" ] ++ attrs)


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea =
    control Html.textarea


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



-- INTERNAL --


control :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
control element attributes children =
    fieldset [ class "form-group" ]
        [ element (class "form-control" :: attributes) children ]
