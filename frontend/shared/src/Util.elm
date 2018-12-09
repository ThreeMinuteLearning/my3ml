module Util exposing (appendErrors, defaultHttpErrorMsg, maybeView, printButton, viewIf, viewUnless)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content

    else
        Html.text ""


viewUnless : Bool -> Html msg -> Html msg
viewUnless condition content =
    viewIf (not condition) content


maybeView : (a -> Html msg) -> Maybe a -> Html msg
maybeView f a_ =
    case a_ of
        Just a ->
            f a

        Nothing ->
            text ""


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }


printButton : msg -> String -> Html msg
printButton print caption =
    a [ class "hidden-print", onClick print, href "#" ] [ text caption ]


defaultHttpErrorMsg : Http.Error -> String
defaultHttpErrorMsg err =
    case err of
        Http.Timeout ->
            "The server took too long to respond. Please try again later"

        Http.NetworkError ->
            "Unable to contact the server. Please check that your network is OK"

        Http.BadStatus { status } ->
            case status.code of
                409 ->
                    "The server detected some conflict. Please report an error"

                403 ->
                    "The server prevented access to a resource"

                _ ->
                    "Sorry, there was an error processing the request"

        Http.BadPayload _ _ ->
            "Oops. Looks like there might be a bug in the app. Couldn't decode the server response"

        Http.BadUrl _ ->
            "Couldn't send the request because the URL was wrong (shouldn't happen :-/)"
