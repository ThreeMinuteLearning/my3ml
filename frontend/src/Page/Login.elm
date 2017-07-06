module Page.Login exposing (view, update, Model, Msg, initialModel)

{-| The login page.
-}

import Api
import Data.Session as Session exposing (Session, User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route exposing (Route)
import Util exposing ((=>))
import Validate exposing (..)
import Views.Form as Form


-- MODEL --


type alias Model =
    { errors : List Error
    , username : String
    , password : String
    }


initialModel : Model
initialModel =
    { errors = []
    , username = ""
    , password = ""
    }



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "auth-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
                    , p [ class "text-xs-center" ]
                        [ a [ Route.href Route.Register ]
                            [ text "Need an account?" ]
                        ]
                    , Form.viewErrors model.errors
                    , viewForm
                    ]
                ]
            ]
        ]


viewForm : Html Msg
viewForm =
    Html.form [ onSubmit SubmitForm ]
        [ Form.input
            [ class "form-control-lg"
            , placeholder "Username"
            , tabindex 1
            , onInput SetUsername
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Password"
            , tabindex 2
            , onInput SetPassword
            ]
            []
        , button [ class "btn btn-lg btn-primary pull-xs-right", tabindex 3 ]
            [ text "Sign in" ]
        ]



-- UPDATE --


type Msg
    = SubmitForm
    | SetUsername String
    | SetPassword String
    | LoginCompleted (Result Http.Error Api.Login)


update : Msg -> Model -> ( ( Model, Cmd Msg ), Maybe Api.Login )
update msg model =
    case msg of
        SubmitForm ->
            case validate model of
                [] ->
                    { model | errors = [] }
                        => Http.send LoginCompleted (Api.postAuthenticate (Api.LoginRequest (.username model) (.password model)))
                        => Nothing

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => Nothing

        SetUsername username ->
            { model | username = username }
                => Cmd.none
                => Nothing

        SetPassword password ->
            { model | password = password }
                => Cmd.none
                => Nothing

        LoginCompleted (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            case response.status.code of
                                401 ->
                                    [ "Username or password is incorrect" ]

                                500 ->
                                    [ "Ooops! We had trouble processing your sign-in request." ]

                                _ ->
                                    [ response.status.message ]

                        _ ->
                            [ "Couldn't process sign-in request" ]
            in
                { model | errors = List.map (\errorMessage -> Form => errorMessage) errorMessages }
                    => Cmd.none
                    => Nothing

        LoginCompleted (Ok user) ->
            model
                => Cmd.none
                => Just user



-- VALIDATION --


type Field
    = Form
    | Email
    | Password


type alias Error =
    ( Field, String )


validate : Model -> List Error
validate =
    Validate.all
        [ .username >> ifBlank (Email => "email can't be blank.")
        , .password >> ifBlank (Password => "password can't be blank.")
        ]
