module Login exposing (..)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Navigation


type User
    = Guest
    | User String UserType String


type UserType
    = Student
    | Teacher
    | Editor
    | Admin


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


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


type Msg
    = UsernameInput String
    | PasswordInput String
    | Submit
    | Error String
    | LoginResponse (Result Http.Error Api.Login)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe User )
update msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none, Nothing )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none, Nothing )

        Submit ->
            let
                request =
                    Api.postAuthenticate (Api.LoginRequest model.username model.password)

                cmd =
                    Http.send LoginResponse request
            in
                ( model, cmd, Nothing )

        Error error ->
            ( { model | error = Just error }, Cmd.none, Nothing )

        LoginResponse (Ok lr) ->
            ( initModel, Navigation.newUrl "#/", Just (loginResponseToUser lr) )

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


loginResponseToUser : Api.Login -> User
loginResponseToUser login =
    let
        userType =
            case .userType login.role of
                "Teacher" ->
                    Teacher

                "Editor" ->
                    Editor

                "Admin" ->
                    Admin

                _ ->
                    Student
    in
        User login.name userType (.accessToken login.token)


view : Model -> Html Msg
view model =
    div [ class "login" ]
        [ errorPanel model.error
        , loginForm model
        ]


loginForm : Model -> Html Msg
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
