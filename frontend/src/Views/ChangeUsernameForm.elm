module Views.ChangeUsernameForm exposing (Model, Msg, ExternalMsg(..), init, update, view)

import Api
import Data.Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Regex exposing (Regex)
import Util exposing ((=>), defaultHttpErrorMsg)
import Validate exposing (Validator, ifInvalid)
import Views.Form as Form


minLength : Int
minLength =
    3


type alias Model =
    { errors : List Error
    , subject : String
    , username : String
    }


init : String -> Model
init subjectId =
    Model [] subjectId ""


type Msg
    = SubmitForm
    | SetUsername String
    | UsernameUpdateResponse (Result Http.Error Api.NoContent)


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
                        => sendUsernameChangeRequest session model
                        => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp

        SetUsername username ->
            { model | username = username }
                => Cmd.none
                => NoOp

        UsernameUpdateResponse (Ok _) ->
            model
                => Cmd.none
                => Completed

        UsernameUpdateResponse (Err e) ->
            (case e of
                Http.BadStatus { status } ->
                    case status.code of
                        409 ->
                            addError model ("This user name is already taken")

                        _ ->
                            addError model (defaultHttpErrorMsg e)

                _ ->
                    addError model (defaultHttpErrorMsg e)
            )
                => Cmd.none
                => NoOp


addError : Model -> Error -> Model
addError model e =
    { model | errors = e :: model.errors }


sendUsernameChangeRequest : Session -> Model -> Cmd Msg
sendUsernameChangeRequest session model =
    Api.postSchoolStudentsByStudentIdUsername (authorization session) model.subject model.username
        |> Http.send UsernameUpdateResponse


type alias Error =
    String


spaceChars : Regex
spaceChars =
    Regex.regex "\\s+"


validate : Model -> List Error
validate =
    Validate.all
        [ .username
            >> (ifNotLongEnough minLength)
                ("Username must be at least " ++ toString minLength ++ " characters")
        , .username
            >> ifInvalid (Regex.contains spaceChars) "Username can't contain spaces"
        ]


ifNotLongEnough : Int -> Error -> String -> List Error
ifNotLongEnough l =
    ifInvalid (\s -> String.length s < l)


view : Model -> Html Msg
view model =
    let
        viewForm =
            Html.form [ onSubmit SubmitForm ]
                [ Form.input
                    [ class "form-control-lg"
                    , placeholder "New username"
                    , tabindex 1
                    , onInput SetUsername
                    ]
                    []
                , submitButton
                ]

        submitButton =
            Html.button [ class "btn btn-primary pull-xs-right", tabindex 2 ] [ text "Save new username" ]
    in
        div []
            [ p [] [ text "Please avoid using personal names (or variations of them) as usernames." ]
            , Form.viewErrorMsgs model.errors
            , viewForm
            ]
