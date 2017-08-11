module Views.ChangePasswordForm exposing (Model, Msg, ExternalMsg(..), init, update, view)

import Api
import Data.Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Util exposing ((=>), defaultHttpErrorMsg)
import Validate exposing (Validator, ifInvalid)
import Views.Form as Form


type alias Model =
    { errors : List Error
    , subject : String
    , minLength : Int
    , password : String
    , confirmPassword : String
    }


init : String -> Int -> Model
init subjectId l =
    Model [] subjectId l "" ""


type Msg
    = SubmitForm
    | SetPassword String
    | SetConfirm String
    | PasswordUpdateResponse (Result Http.Error Api.NoContent)


type ExternalMsg
    = NoOp
    | Completed


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        SubmitForm ->
            case validate model of
                [] ->
                    { model | errors = [] }
                        => sendPasswordChangeRequest session model
                        => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp

        SetPassword password ->
            { model | password = password }
                => Cmd.none
                => NoOp

        SetConfirm password ->
            { model | confirmPassword = password }
                => Cmd.none
                => NoOp

        PasswordUpdateResponse (Ok _) ->
            model
                => Cmd.none
                => Completed

        PasswordUpdateResponse (Err e) ->
            { model | errors = ("Password update failed: " ++ defaultHttpErrorMsg e) :: model.errors }
                => Cmd.none
                => NoOp


sendPasswordChangeRequest : Session -> Model -> Cmd Msg
sendPasswordChangeRequest session model =
    Api.postSchoolStudentsByStudentIdPassword (authorization session) model.subject model.password
        |> Http.send PasswordUpdateResponse


type alias Error =
    String


validate : Model -> List Error
validate =
    Validate.all
        [ \m ->
            (ifNotLongEnough m.minLength)
                ("Password must be at least " ++ toString m.minLength ++ " characters")
                m.password
        , ifInvalid (\m -> m.password /= m.confirmPassword) ("Passwords don't match")
        ]


ifNotLongEnough : Int -> Error -> String -> List Error
ifNotLongEnough l =
    ifInvalid (\s -> String.length s < l)


view : Model -> Html Msg
view model =
    let
        viewForm =
            Html.form [ onSubmit SubmitForm ]
                [ Form.password
                    [ class "form-control-lg"
                    , placeholder "Password"
                    , tabindex 1
                    , onInput SetPassword
                    ]
                    []
                , Form.password
                    [ class "form-control-lg"
                    , placeholder "Confirm password"
                    , tabindex 2
                    , onInput SetConfirm
                    ]
                    []
                , submitButton
                ]

        submitButton =
            Html.button [ class "btn btn-primary pull-xs-right", tabindex 3 ] [ text "Save new password" ]
    in
        div []
            [ Form.viewErrorMsgs model.errors
            , viewForm
            ]
