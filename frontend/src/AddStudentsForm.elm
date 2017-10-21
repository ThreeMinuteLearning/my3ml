module AddStudentsForm exposing (Model, Msg, init, update, view)

import Api
import Bootstrap exposing (errorClass)
import Data.Session exposing (Session, authorization)
import Exts.List exposing (firstMatch)
import Html exposing (..)
import Html.Attributes exposing (class, id, selected)
import Html.Events exposing (onInput, onSubmit)
import Http
import Regex
import Util exposing ((=>), defaultHttpErrorMsg)
import Validate exposing (Validator, ifBlank, ifInvalid)
import Views.Form as Form
import Views.SelectLevel as SelectLevel


type alias Model =
    { errors : List Error
    , level : Int
    , class : Maybe String
    , names : String
    }


init : Model
init =
    { errors = []
    , level = 3
    , class = Nothing
    , names = ""
    }


type Msg
    = SubmitForm
    | SetClass (Maybe String)
    | SetLevel Int
    | SetNames String
    | AddStudentsResponse (Result Http.Error (List NewAccount))


type alias NewAccount =
    ( Api.Student, ( String, String ) )


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Maybe (List NewAccount) )
update session msg model =
    case msg of
        SubmitForm ->
            case validate model of
                Ok names ->
                    { model | errors = [] }
                        => sendNewAccountsRequest session model.level names
                        => Nothing

                Err errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => Nothing

        SetClass class ->
            { model | class = class }
                => Cmd.none
                => Nothing

        SetLevel level ->
            { model | level = level }
                => Cmd.none
                => Nothing

        SetNames names ->
            { model | names = names }
                => Cmd.none
                => Nothing

        AddStudentsResponse (Ok newAccounts) ->
            ( model, Cmd.none ) => Just newAccounts

        AddStudentsResponse (Err e) ->
            { model | errors = model.errors ++ [ ("Couldn't save the new accounts: " ++ defaultHttpErrorMsg e) ] }
                => Cmd.none
                => Nothing


sendNewAccountsRequest : Session -> Int -> List String -> Cmd Msg
sendNewAccountsRequest session level names =
    Api.postSchoolStudents (authorization session) ( level, names )
        |> Http.send AddStudentsResponse


type alias Error =
    String


validate : Model -> Result (List Error) (List String)
validate model =
    let
        parsedNames =
            Regex.split Regex.All (Regex.regex "[,\\r\\n]+") model.names
                |> List.map String.trim

        validateName n =
            if not (validName n) then
                [ n ++ " is not a valid name" ]
            else
                []

        errors =
            List.map validateName parsedNames
                |> List.concat
    in
        case errors of
            [] ->
                Ok parsedNames

            _ ->
                Err errors


validName : String -> Bool
validName name =
    String.length (String.trim name)
        |> \l -> l > 2 && l < 50


view : Model -> Html Msg
view model =
    let
        namesInput =
            div
                [ class
                    (if List.isEmpty model.errors then
                        ""
                     else
                        "has-error"
                    )
                ]
                [ Form.textarea [ onInput SetNames ] []
                ]

        submitButton =
            Html.button [ class "btn btn-primary pull-xs-right" ] [ text "Create Accounts" ]

        viewForm =
            Html.form
                [ onSubmit SubmitForm ]
                [ Html.fieldset []
                    ([ namesInput, SelectLevel.view SetLevel model.level, submitButton ])
                ]
    in
        div []
            [ Form.viewErrorMsgs model.errors
            , viewForm
            ]
