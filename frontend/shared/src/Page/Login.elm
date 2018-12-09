module Page.Login exposing (Model, Msg, initialModel, update, view)

{-| The login page.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Util exposing (defaultHttpErrorMsg)
import Validate exposing (..)
import Views.Form as Form


type alias Model =
    { errors : List Error
    , username : String
    , password : String
    , otp : Maybe String
    , otpRequired : Bool
    }


initialModel : Model
initialModel =
    { errors = []
    , username = ""
    , password = ""
    , otp = Nothing
    , otpRequired = False
    }


view : Model -> Maybe (Attribute (Msg a)) -> Html (Msg a)
view model regLink =
    div [ class "auth-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
                    , case regLink of
                        Just ref ->
                            p [ class "text-xs-center" ]
                                [ a [ ref ]
                                    [ text "Need an account?" ]
                                ]

                        Nothing ->
                            text ""
                    , Form.viewErrors model.errors
                    , if model.otpRequired then
                        viewOtpForm model

                      else
                        viewForm
                    ]
                ]
            ]
        ]


viewForm : Html (Msg a)
viewForm =
    Html.form [ id "login-form", onSubmit SubmitForm ]
        [ Form.input
            [ class "form-control-lg"
            , id "username"
            , name "username"
            , placeholder "Username or email"
            , tabindex 1
            , onInput SetUsername
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , id "password"
            , placeholder "Password"
            , tabindex 2
            , onInput SetPassword
            ]
            []
        , button [ class "btn btn-lg btn-primary pull-xs-right", tabindex 3 ]
            [ text "Sign in" ]
        ]


viewOtpForm : Model -> Html (Msg a)
viewOtpForm model =
    Html.form [ id "otp-form", onSubmit SubmitForm ]
        [ Form.input
            [ class "form-control-lg"
            , id "otp-code"
            , name "otp-code"
            , value (Maybe.withDefault "" model.otp)
            , placeholder "One-time password code"
            , tabindex 1
            , onInput SetOTP
            ]
            []
        , button [ class "btn btn-lg btn-primary pull-xs-right", tabindex 3 ]
            [ text "Sign in" ]
        ]


type Msg a
    = SubmitForm
    | SetUsername String
    | SetPassword String
    | SetOTP String
    | LoginCompleted (Result Http.Error a)


update : Msg a -> Model -> (String -> String -> Maybe Int -> Http.Request a) -> ( ( Model, Cmd (Msg a) ), Maybe a )
update msg model loginRequest =
    case msg of
        SubmitForm ->
            case validate validator model of
                Err errors ->
                    ( ( { model | errors = errors }, Cmd.none ), Nothing )

                _ ->
                    ( ( { model | errors = [] }
                      , Http.send LoginCompleted (loginRequest (String.trim model.username) model.password (Maybe.andThen String.toInt model.otp))
                      )
                    , Nothing
                    )

        SetUsername username ->
            ( ( { model | username = username }, Cmd.none ), Nothing )

        SetPassword password ->
            ( ( { model | password = password }, Cmd.none ), Nothing )

        SetOTP otp ->
            ( ( { model | otp = Just otp }, Cmd.none ), Nothing )

        LoginCompleted (Err ((Http.BadStatus response) as error)) ->
            case response.status.code of
                401 ->
                    loginError "Login failed. Check your username and password" model

                403 ->
                    loginError "Please wait till your account is enabled before signing in" model

                429 ->
                    loginError "The login server is a bit busy. Please try again" model

                462 ->
                    ( ( { model | otpRequired = True }, Cmd.none ), Nothing )

                _ ->
                    loginError (defaultHttpErrorMsg error) model

        LoginCompleted (Err error) ->
            loginError (defaultHttpErrorMsg error) model

        LoginCompleted (Ok user) ->
            ( ( model, Cmd.none ), Just user )


loginError : String -> Model -> ( ( Model, Cmd (Msg a) ), Maybe a )
loginError errorMsg model =
    ( ( { model | errors = [ ( Form, errorMsg ) ], otp = Nothing, otpRequired = False }, Cmd.none ), Nothing )


type Field
    = Form
    | Email
    | Password
    | OTP


type alias Error =
    ( Field, String )


validator : Validator Error Model
validator =
    Validate.all
        [ ifBlank .username ( Email, "You must enter a username" )
        , ifBlank .password ( Password, "You must enter a password" )
        , ifTrue (\m -> m.otpRequired && Maybe.andThen String.toInt m.otp == Nothing) ( OTP, "You must enter a number for the one-time password" )
        ]
