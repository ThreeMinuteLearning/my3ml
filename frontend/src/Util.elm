module Util exposing ((=>), pair, onClickStopPropagation, onClickPreventDefault, viewIf, appendErrors, dialog, printButton, defaultHttpErrorMsg)

import Dialog
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions, onClick, defaultOptions)
import Http
import Json.Decode as Decode


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>


{-| Useful when building up a Cmd via a pipeline, and then pairing it with
a model at the end.

    session.user
        |> User.Request.foo
        |> Task.attempt Foo
        |> pair { model | something = blah }

-}
pair : a -> b -> ( a, b )
pair first second =
    first => second


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        Html.text ""


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (Decode.succeed msg)


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    onWithOptions "click"
        { defaultOptions | stopPropagation = True }
        (Decode.succeed msg)


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }


dialog : msg -> Maybe (Html msg) -> Html msg -> Dialog.Config msg
dialog closeMsg hdr body =
    { closeMessage = Just closeMsg
    , containerClass = Nothing
    , header = hdr
    , body = Just body
    , footer = Nothing
    }


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

                _ ->
                    "Sorry, there was an error processing the request"

        Http.BadPayload _ _ ->
            "Oops. Looks like there might be a bug in the app. Couldn't decode the server response"

        Http.BadUrl _ ->
            "Couldn't send the request because the URL was wrong (shouldn't happen :-/)"
