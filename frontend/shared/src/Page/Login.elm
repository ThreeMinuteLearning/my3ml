module Page.Login exposing (Model, Msg, initialModel, update, view)

{-| The login page.
-}

import Components
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Util exposing (defaultHttpErrorMsg)
import Validate exposing (..)
import Views.Form as Form


type State
    = Init
    | Loading
    | Success


type alias Model =
    { errors : List Error
    , username : String
    , password : String
    , otp : Maybe String
    , otpRequired : Bool
    , state : State
    }


initialModel : Model
initialModel =
    { errors = []
    , username = ""
    , password = ""
    , otp = Nothing
    , otpRequired = False
    , state = Init
    }


view : Model -> Maybe (Attribute (Msg a)) -> Html (Msg a)
view model regLink =
    div [ class "py-10 px-4 flex justify-center" ]
        [ div [ class "w-full max-w-md" ]
            [ Form.viewErrors model.errors
            , if model.otpRequired then
                viewOtpForm model

              else
                viewForm (model.state == Loading) regLink
            ]
        ]


form_ : String -> List (Html (Msg a)) -> Html (Msg a)
form_ formId =
    Html.form [ id formId, class "bg-white shadow-md rounded px-8 pt-6 pb-8 sm:p-16 mb-4", onSubmit SubmitForm ]


submitButton : Bool -> Html msg
submitButton isDisabled =
    Components.btn [ type_ "submit", tabindex 3, disabled isDisabled ] [ text "Sign in" ]


viewForm : Bool -> Maybe (Attribute (Msg a)) -> Html (Msg a)
viewForm isDisabled regLink =
    form_ "login-form"
        [ div [ class "mb-4" ]
            [ Form.label [ class "mb-2", for "username" ] [ text "Username" ]
            , Form.input
                [ id "username"
                , class "w-full"
                , name "username"
                , disabled isDisabled
                , placeholder "Username or email"
                , tabindex 1
                , onInput SetUsername
                ]
                []
            ]
        , div [ class "mb-6" ]
            [ Form.label [ class "mb-2", for "password" ] [ text "Password" ]
            , Form.password
                [ id "password"
                , class "w-full"
                , name "password"
                , disabled isDisabled
                , placeholder "Password"
                , tabindex 2
                , onInput SetPassword
                ]
                []
            ]
        , div [ class "flex items-center justify-between" ]
            [ submitButton isDisabled
            , case regLink of
                Just ref ->
                    a [ class "font-bold text-sm text-blue-500 hover:text-blue-700", ref ]
                        [ text "Need an account?" ]

                Nothing ->
                    text ""
            ]
        ]


viewOtpForm : Model -> Html (Msg a)
viewOtpForm model =
    form_ "otp-form"
        [ Form.input
            [ id "otp-code"
            , name "otp-code"
            , disabled (model.state == Loading)
            , value (Maybe.withDefault "" model.otp)
            , placeholder "One-time password code"
            , tabindex 1
            , onInput SetOTP
            ]
            []
        , submitButton (model.state == Loading)
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
                    if model.state == Init then
                        ( ( { model | state = Loading, errors = [] }
                          , Http.send LoginCompleted (loginRequest (String.trim model.username) model.password (Maybe.andThen String.toInt model.otp))
                          )
                        , Nothing
                        )

                    else
                        ( ( model, Cmd.none ), Nothing )

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
                    ( ( { model | state = Init, otpRequired = True }, Cmd.none ), Nothing )

                _ ->
                    loginError (defaultHttpErrorMsg error) model

        LoginCompleted (Err error) ->
            loginError (defaultHttpErrorMsg error) model

        LoginCompleted (Ok user) ->
            ( ( { model | state = Success }, Cmd.none ), Just user )


loginError : String -> Model -> ( ( Model, Cmd (Msg a) ), Maybe a )
loginError errorMsg model =
    ( ( { model | errors = [ ( Form, errorMsg ) ], state = Init, otp = Nothing, otpRequired = False }, Cmd.none ), Nothing )


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
