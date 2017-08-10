module Page.Register exposing (Model, Msg, init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput)
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
    , schoolName : String
    , teacherName : String
    , password : String
    , confirmPassword : String
    , completed : Bool
    }


type Msg
    = SubmitForm
    | SetEmail String
    | SetSchoolName String
    | SetTeacherName String
    | SetPassword String
    | SetConfirmPassword String
    | RegisterResponse (Result Http.Error Api.NoContent)


init : Session -> Model
init session =
    { errors = []
    , email = ""
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
                        => (Api.postAccountRegister (authorization session) (Api.Registration model.email model.schoolName model.teacherName model.password)
                                |> Http.send RegisterResponse
                           )

                errors ->
                    { model | errors = errors }
                        => Cmd.none

        SetEmail email ->
            { model | email = email }
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

        RegisterResponse (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus { status } ->
                            case status.code of
                                409 ->
                                    [ "Account already registered" ]

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
            [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                [ h1 [ class "text-xs-center" ] [ text "Sign up" ]
                , p [ class "text-xs-center" ]
                    [ a [ Route.href Route.Login ]
                        [ text "Have an account?" ]
                    ]
                , Form.viewErrors model.errors
                , if model.completed then
                    p [] [ text "Registration complete. Someone will be in touch when your account has been activated." ]
                  else
                    viewForm
                ]
            ]
        ]


viewForm : Html Msg
viewForm =
    Html.form [ onSubmit SubmitForm ]
        [ Form.input
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
