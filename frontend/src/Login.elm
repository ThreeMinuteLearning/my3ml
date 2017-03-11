module Login exposing (Model, Msg(..), init, subscriptions, update, view)

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


init : ( Model, Cmd (Msg u) )
init =
    ( initModel, Cmd.none )


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
                                    resp.body

                                _ ->
                                    resp.status.message

                        _ ->
                            "Login Error!"
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
    Html.form [ onSubmit Submit ]
        [ fieldset []
            [ legend [] [ text "Login" ]
            , div []
                [ label [] [ text "Username" ]
                , input
                    [ type_ "text"
                    , value model.username
                    , onInput UsernameInput
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Password" ]
                , input
                    [ type_ "password"
                    , value model.password
                    , onInput PasswordInput
                    ]
                    []
                ]
            , div []
                [ label [] []
                , button [ type_ "submit" ] [ text "Login" ]
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
subscriptions model =
    Sub.none
