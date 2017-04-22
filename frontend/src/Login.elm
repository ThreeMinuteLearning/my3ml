module Login exposing (Model, Msg(..), initModel, subscriptions, update, view)

import Exts.Html.Bootstrap exposing (formGroup)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Navigation


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    , error = Nothing
    }


type Msg u
    = UsernameInput String
    | PasswordInput String
    | Submit
    | Error String
    | LoginResponse (Result Http.Error u)


update : (String -> String -> Http.Request u) -> Msg u -> Model -> ( Model, Cmd (Msg u), Maybe u )
update mkRequest msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none, Nothing )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none, Nothing )

        Submit ->
            let
                request =
                    mkRequest model.username model.password

                cmd =
                    Http.send LoginResponse request
            in
                ( initModel, cmd, Nothing )

        Error error ->
            ( { model | error = Just error }, Cmd.none, Nothing )

        LoginResponse (Ok u) ->
            ( initModel, Navigation.newUrl "#/", Just u )

        LoginResponse (Err err) ->
            let
                errMsg =
                    case err of
                        Http.BadStatus resp ->
                            case resp.status.code of
                                401 ->
                                    "Login failed"

                                _ ->
                                    resp.status.message

                        _ ->
                            "Login Error"
            in
                ( { model | error = Just errMsg }, Cmd.none, Nothing )


view : Model -> Html (Msg u)
view model =
    div [ class "login" ]
        [ errorPanel model.error
        , loginForm model
        ]


loginForm : Model -> Html (Msg u)
loginForm model =
    Html.form [ class "form-horizontal", onSubmit Submit ]
        [ formGroup
            [ label [ class "col-sm-2 control-label", for "username" ] [ text "Username" ]
            , div [ class "col-sm-4" ]
                [ input
                    [ type_ "text"
                    , value model.username
                    , onInput UsernameInput
                    , class "form-control"
                    , id "username"
                    ]
                    []
                ]
            ]
        , formGroup
            [ label [ class "col-sm-2 control-label", for "password" ] [ text "Password" ]
            , div [ class "col-sm-4" ]
                [ input
                    [ type_ "password"
                    , value model.password
                    , onInput PasswordInput
                    , class "form-control"
                    , id "password"
                    ]
                    []
                ]
            ]
        , formGroup
            [ div [ class "col-sm-offset-2 col-sm-4" ]
                [ button [ class "btn btn-primary", type_ "submit" ] [ text "Login" ]
                ]
            ]
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg ]


subscriptions : Model -> Sub (Msg u)
subscriptions _ =
    Sub.none
