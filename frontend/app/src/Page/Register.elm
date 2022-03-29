module Page.Register exposing (Model, Msg, init, update, view)

import Api
import Components
import Data.Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Route
import SHA1
import Util exposing (defaultHttpErrorMsg)
import Validate exposing (Validator, fromErrors, ifBlank, ifInvalidEmail, validate)
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
    , status : Status
    }


type PwnedCount
    = Zero
    | One
    | Pwned Int
    | Error


type Status
    = NotSent
    | AwaitingPwnedResponse
    | AwaitingResponse
    | Completed


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
    | PwnedResults (Result Http.Error String)


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
    , status = NotSent
    }


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SubmitForm ->
            case validate validator model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none )

                _ ->
                    ( { model | status = AwaitingPwnedResponse, errors = [] }, getPwnedMatches model.password )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetCode code ->
            ( { model | code = Just code, schoolName = code }, Cmd.none )

        SetSchoolName name ->
            ( { model | schoolName = name }, Cmd.none )

        SetTeacherName name ->
            ( { model | teacherName = name }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        SetConfirmPassword password ->
            ( { model | confirmPassword = password }, Cmd.none )

        SetRegistrationType t ->
            ( { model | registrationType = Just t }, Cmd.none )

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
            ( { model | status = NotSent, errors = List.map (\errorMessage -> ( Form, errorMessage )) errorMessages }, Cmd.none )

        RegisterResponse (Ok _) ->
            ( { model | status = Completed }, Cmd.none )

        PwnedResults (Err _) ->
            sendForm model session

        PwnedResults (Ok matches) ->
            let
                pwnedCount =
                    pwnedCountFromResponse model.password matches

                errors =
                    validatePwned pwnedCount
            in
            if errors == [] then
                sendForm model session

            else
                ( { model | errors = errors, status = NotSent }, Cmd.none )


sendForm : Model -> Session -> ( Model, Cmd Msg )
sendForm model session =
    ( { model | status = AwaitingResponse, errors = [] }
    , Api.postAccountRegister (authorization session) (Api.Registration model.email model.code model.schoolName model.teacherName model.password)
        |> Http.send RegisterResponse
    )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register with 3ml"
    , content =
        div [ class "max-w-xl mx-auto flex flex-col" ]
            [ h1 [ class "text-xs-center text-gray-600" ] [ text (heading model.registrationType) ]
            , p [ class "my-2 text-lg leading-normal" ]
                [ text (blurb model.registrationType)
                ]
            , p [ class "mb-4" ]
                [ a [ Route.href Route.Login, class "text-blue-500 hover:text-blue-700" ]
                    [ text "Have an account already?" ]
                ]
            , Form.viewErrors model.errors
            , if model.registrationType == Nothing then
                viewRegistrationOptions

              else if model.status == Completed then
                div [ class "text-lg" ]
                    [ p [] [ text "Registration complete." ]
                    , p []
                        [ case model.registrationType of
                            Just WithCode ->
                                text "Your school admin should let you know when they have activated your account."

                            _ ->
                                text "Your account should be activated within an hour."
                        ]
                    , p [] [ text "You can then sign in using your email and password." ]
                    ]

              else
                viewForm model
            ]
    }


heading : Maybe RegistrationType -> String
heading rt =
    case rt of
        Nothing ->
            "Sign up"

        Just NewSchool ->
            "Register a new school"

        Just WithCode ->
            "Register as a teacher in an existing school"


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
    div [ class "flex justify-around" ]
        [ Components.btn [ id "register-school", class "mr-2", type_ "button", onClick (SetRegistrationType NewSchool) ] [ text "Register a new school" ]
        , Components.btn [ id "register-teacher", type_ "button", onClick (SetRegistrationType WithCode) ] [ text "Register as a teacher in an existing school" ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ class "flex flex-col", onSubmit SubmitForm ]
        [ case model.registrationType of
            Just WithCode ->
                Form.input
                    [ class ""
                    , placeholder "Registration code"
                    , onInput SetCode
                    , maxlength 100
                    ]
                    []

            _ ->
                Form.input
                    [ class ""
                    , placeholder "School name"
                    , onInput SetSchoolName
                    , maxlength 100
                    ]
                    []
        , Form.input
            [ class "my-2"
            , placeholder "Your name"
            , onInput SetTeacherName
            , maxlength 100
            ]
            []
        , Form.input
            [ class "mb-2"
            , placeholder "Email"
            , onInput SetEmail
            , maxlength 50
            ]
            []
        , Form.password
            [ class ""
            , placeholder "Choose a password"
            , onInput SetPassword
            , maxlength 100
            , type_ "password"
            ]
            []
        , Form.password
            [ class "my-2"
            , placeholder "Confirm Password"
            , onInput SetConfirmPassword
            , maxlength 100
            ]
            []
        , Components.btn [ class "mt-1", disabled (model.status /= NotSent) ]
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


validator : Validator Error Model
validator =
    Validate.all
        [ ifBlank .schoolName ( SchoolName, "school name can't be blank." )
        , ifInvalidEmail .email (\_ -> ( Email, "please enter a valid email." ))
        , ifBlank .teacherName ( TeacherName, "name can't be blank." )
        , fromErrors validatePassword
        ]


validatePassword : Model -> List Error
validatePassword { password, confirmPassword } =
    case password of
        "" ->
            [ ( Password, "password can't be blank" ) ]

        _ ->
            if password /= confirmPassword then
                [ ( Form, "the passwords must be the same" ) ]

            else if String.length password < 8 then
                [ ( Password, "password must be at least 8 characters" ) ]

            else
                []


validatePwned : PwnedCount -> List Error
validatePwned pwned =
    case pwned of
        Zero ->
            []

        One ->
            [ ( Password, "Insecure password (found in a compromised database)!" ) ]

        Pwned n ->
            [ ( Password, "Insecure password (found " ++ String.fromInt n ++ " times in compromised databases)!" ) ]

        Error ->
            []


sha1 : String -> String
sha1 s =
    SHA1.fromString s
        |> SHA1.toHex
        |> String.toUpper


pwnedCountFromResponse : String -> String -> PwnedCount
pwnedCountFromResponse password response =
    let
        suffix =
            sha1 password |> String.dropLeft 5

        match =
            String.lines response
                |> List.filter (String.startsWith suffix)
                |> List.head
                |> Maybe.map (String.dropLeft 36)
                |> Maybe.andThen String.toInt
    in
    case match of
        Nothing ->
            Zero

        Just 1 ->
            One

        Just count ->
            Pwned count


getPwnedMatches : String -> Cmd Msg
getPwnedMatches password =
    Http.send PwnedResults <|
        Http.getString ("https://api.pwnedpasswords.com/range/" ++ String.left 5 (sha1 password))
