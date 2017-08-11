module AddStudentsForm exposing (Model, Msg, init, update, view)

import Api
import Bootstrap exposing (errorClass)
import Data.Session exposing (Session, authorization)
import Exts.List exposing (firstMatch)
import Html exposing (..)
import Html.Attributes exposing (class, id, selected)
import Html.Events exposing (onInput, onSubmit)
import Http
import Util exposing ((=>), defaultHttpErrorMsg)
import Validate exposing (Validator, ifBlank, ifInvalid)
import Views.Form as Form


type alias Model =
    { errors : List Error
    , level : Int
    , class : Maybe String
    , names : List String
    }


init : Model
init =
    { errors = []
    , level = 5
    , class = Nothing
    , names = List.repeat 10 ""
    }


type Msg
    = SubmitForm
    | SetClass (Maybe String)
    | SetLevel String
    | SetName Int String
    | AddStudentsResponse (Result Http.Error (List NewAccount))


type alias NewAccount =
    ( Api.Student, ( String, String ) )


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Maybe (List NewAccount) )
update session msg model =
    case msg of
        SubmitForm ->
            case validate model of
                [] ->
                    { model | errors = [] }
                        => sendNewAccountsRequest session model
                        => Nothing

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => Nothing

        SetClass class ->
            { model | class = class }
                => Cmd.none
                => Nothing

        SetLevel level ->
            { model | level = Result.withDefault (model.level) (String.toInt level) }
                => Cmd.none
                => Nothing

        SetName index name ->
            { model | names = setNameAtIndex index name model.names }
                => Cmd.none
                => Nothing

        AddStudentsResponse (Ok newAccounts) ->
            ( model, Cmd.none ) => Just newAccounts

        AddStudentsResponse (Err e) ->
            { model | errors = model.errors ++ [ (Form => "Couldn't save the new accounts: " ++ defaultHttpErrorMsg e) ] }
                => Cmd.none
                => Nothing


setNameAtIndex : Int -> String -> List String -> List String
setNameAtIndex index name =
    List.indexedMap
        (\i n ->
            if i == index then
                name
            else
                n
        )


sendNewAccountsRequest : Session -> Model -> Cmd Msg
sendNewAccountsRequest session model =
    List.filter (not << String.isEmpty) model.names
        |> Api.postSchoolStudents (authorization session)
        |> Http.send AddStudentsResponse


type Field
    = Form
    | Name Int


type alias Error =
    ( Field, String )


fieldError : Field -> List Error -> Maybe Error
fieldError field errors =
    firstMatch (\e -> Tuple.first e == field) errors


validate : Model -> List Error
validate model =
    let
        validateName index =
            if index == 0 then
                ifInvalid (not << validName) (Name 0 => "You must enter at least one valid name")
            else
                ifInvalid (\n -> not (String.isEmpty n || validName n)) (Name index => "Invalid name")
    in
        List.indexedMap validateName model.names
            |> List.concat


validName : String -> Bool
validName name =
    String.length (String.trim name)
        |> \l -> l > 2 && l < 50


view : Model -> Html Msg
view model =
    let
        nameInput index =
            div [ class (errorClass (fieldError (Name index) model.errors)) ]
                [ Form.input [ onInput (SetName index) ] []
                ]

        nameFields =
            List.map nameInput (List.range 0 9)

        submitButton =
            Html.button [ class "btn btn-primary pull-xs-right" ] [ text "Create Accounts" ]

        viewForm =
            Html.form
                [ onSubmit SubmitForm ]
                [ Html.fieldset []
                    (List.append nameFields [ submitButton ])
                ]
    in
        div []
            [ Form.viewErrors model.errors
            , viewForm
            ]


viewLevelSelect : Model -> Html Msg
viewLevelSelect model =
    let
        levelOption lvl =
            option [ selected (model.level == lvl) ] [ text (toString lvl) ]
    in
        Html.select [ onInput SetLevel ]
            (List.map levelOption (List.range 1 10))
