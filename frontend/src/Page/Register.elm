module Page.Register exposing (Model, Msg, init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional)
import Route
import Util exposing ((=>), defaultHttpErrorMsg, viewIf)
import Validate exposing (Validator, ifBlank, ifNothing)
import Views.Form as Form


type alias Model =
    { errors : List Error
    , email : String
    , registrationType : Maybe RegistrationType
    , code : Maybe String
    , schoolName : String
    , teacherName : String
    , password : String
    , confirmPassword : String
    , completed : Bool
    }


type RegistrationType
    = WithCode
    | NewSchool


type Msg
    = SubmitForm
    | SetEmail String
    | SetCode String
    | SetSchoolName String
    | SetRegistrationType RegistrationType
    | SetTeacherName String
    | SetPassword String
    | SetConfirmPassword String
    | RegisterResponse (Result Http.Error Api.NoContent)


init : Model
init =
    { errors = []
    , email = ""
    , registrationType = Nothing
    , code = Nothing
    , schoolName = ""
    , teacherName = ""
    , password = ""
    , confirmPassword = ""
    , completed = False
    }


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SubmitForm ->
            case validate model of
                [] ->
                    { model | errors = [] }
                        => (Api.postAccountRegister (authorization session) (Api.Registration model.email model.code model.schoolName model.teacherName model.password)
                                |> Http.send RegisterResponse
                           )

                errors ->
                    { model | errors = errors }
                        => Cmd.none

        SetEmail email ->
            { model | email = email }
                => Cmd.none

        SetCode code ->
            { model | code = Just code, schoolName = code }
                => Cmd.none

        SetSchoolName name ->
            { model | schoolName = name }
                => Cmd.none

        SetTeacherName name ->
            { model | teacherName = name }
                => Cmd.none

        SetPassword password ->
            { model | password = password }
                => Cmd.none

        SetConfirmPassword password ->
            { model | confirmPassword = password }
                => Cmd.none

        SetRegistrationType t ->
            { model | registrationType = Just t }
                => Cmd.none

        RegisterResponse (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus { status } ->
                            case status.code of
                                409 ->
                                    [ "Account already registered" ]

                                403 ->
                                    [ "The registration code was not found or may have expired" ]

                                _ ->
                                    [ "There was an error during registration" ]

                        _ ->
                            [ defaultHttpErrorMsg error ]
            in
                { model | errors = List.map (\errorMessage -> Form => errorMessage) errorMessages }
                    => Cmd.none

        RegisterResponse (Ok _) ->
            { model | completed = True }
                => Cmd.none


view : Model -> Html Msg
view model =
    div [ class "container page" ]
        [ div [ class "row" ]
            [ div [ class "col-md-12" ]
                [ h1 [ class "text-xs-center" ] [ text "Sign up" ]
                , p []
                    [ text (blurb model.registrationType)
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                [ p [ class "text-xs-center" ]
                    [ a [ Route.href Route.Login ]
                        [ text "Have an account already?" ]
                    ]
                , Form.viewErrors model.errors
                , if model.registrationType == Nothing then
                    viewRegistrationOptions
                  else if model.completed then
                    p [] [ text "Registration complete. Someone will be in touch when your account has been activated." ]
                  else
                    viewForm model
                ]
            ]
        ]


blurb : Maybe RegistrationType -> String
blurb rt =
    case rt of
        Nothing ->
            """
You can either register a new school or, if your school is already using 3ml, as a teacher in an existing school.
"""

        Just WithCode ->
            """To register for an existing school, a 3ml admin at your school should have given
you a registration code. Please enter it in the form below. If you don't have a code, please
ask the teacher who registered your school for one. The code is only valid for a short time, so you
should complete your registration right away.
"""

        Just NewSchool ->
            """
Registering will create a new school in the 3ml system for which you will be the administrator.
You will be able to create accounts for the children in your school to use 3ml and also
accounts for other teachers. You will be responsible for obtaining consent for your students
to use the system, for controlling the data that they enter and for maintaining the accounts
for your school.
"""


viewRegistrationOptions : Html Msg
viewRegistrationOptions =
    div []
        [ button [ type_ "button", class "btn", onClick (SetRegistrationType NewSchool) ] [ text "Register a new school" ]
        , text " "
        , button [ type_ "button", class "btn btn-primary", onClick (SetRegistrationType WithCode) ] [ text "Register as a teacher in an existing school" ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ onSubmit SubmitForm ]
        [ case model.registrationType of
            Just WithCode ->
                Form.input
                    [ class "form-control-lg"
                    , placeholder "Registration code"
                    , onInput SetCode
                    , maxlength 100
                    ]
                    []

            _ ->
                Form.input
                    [ class "form-control-lg"
                    , placeholder "School name"
                    , onInput SetSchoolName
                    , maxlength 100
                    ]
                    []
        , Form.input
            [ class "form-control-lg"
            , placeholder "Your name"
            , onInput SetTeacherName
            , maxlength 100
            ]
            []
        , Form.input
            [ class "form-control-lg"
            , placeholder "Email"
            , onInput SetEmail
            , maxlength 50
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Choose a password"
            , onInput SetPassword
            , maxlength 100
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Confirm Password"
            , onInput SetConfirmPassword
            , maxlength 100
            ]
            []
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Sign up" ]
        ]



-- VALIDATION --


type Field
    = Form
    | SchoolName
    | TeacherName
    | Email
    | Password


type alias Error =
    ( Field, String )


validate : Model -> List Error
validate =
    Validate.all
        [ .schoolName >> ifBlank (SchoolName => "school name can't be blank.")
        , .email >> ifBlank (Email => "email can't be blank.")
        , .email >> ifBlank (TeacherName => "name can't be blank.")
        , validatePassword
        ]


validatePassword : Model -> List Error
validatePassword { password, confirmPassword } =
    case password of
        "" ->
            [ ( Password, "password can't be blank" ) ]

        _ ->
            if password /= confirmPassword then
                [ ( Form, "the passwords must be the same" ) ]
            else
                []
