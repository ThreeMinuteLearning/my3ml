module AddStudentsForm exposing (Model, Msg, init, update, view)

import Api
import Bootstrap exposing (errorClass)
import Components
import Data.Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Regex
import Util exposing (defaultHttpErrorMsg)
import Views.Form as Form
import Views.SelectLevel as SelectLevel


type alias Model =
    { errors : List Error
    , level : Int
    , class : Maybe String
    , names : String
    , waitingForResponse : Bool
    }


init : Model
init =
    { errors = []
    , level = 3
    , class = Nothing
    , names = ""
    , waitingForResponse = False
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
                    ( ( { model | errors = [], waitingForResponse = True }
                      , sendNewAccountsRequest session model.level names
                      )
                    , Nothing
                    )

                Err errors ->
                    ( ( { model | errors = errors }, Cmd.none ), Nothing )

        SetClass class ->
            ( ( { model | class = class }, Cmd.none ), Nothing )

        SetLevel level ->
            ( ( { model | level = level }, Cmd.none ), Nothing )

        SetNames names ->
            ( ( { model | names = names }, Cmd.none ), Nothing )

        AddStudentsResponse (Ok newAccounts) ->
            ( ( { model | waitingForResponse = False }, Cmd.none ), Just newAccounts )

        AddStudentsResponse (Err e) ->
            ( ( { model | errors = model.errors ++ [ "Couldn't save the new accounts: " ++ defaultHttpErrorMsg e ], waitingForResponse = False }
              , Cmd.none
              )
            , Nothing
            )


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
            Regex.split (Maybe.withDefault Regex.never (Regex.fromString "[,\\r\\n]+")) model.names
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)

        validateName n =
            if not (validName n) then
                [ n ++ " is not a valid name" ]

            else
                []

        lengthCheck =
            case List.length parsedNames of
                0 ->
                    [ "Please enter at least one name" ]

                n ->
                    if n < 101 then
                        []

                    else
                        [ "You can only create 100 accounts at a atime" ]

        errors =
            List.map validateName parsedNames
                |> List.concat
                |> List.append lengthCheck
    in
    case errors of
        [] ->
            Ok parsedNames

        _ ->
            Err errors


validName : String -> Bool
validName name =
    String.length (String.trim name)
        |> (\l -> l >= 2 && l < 50)


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
                [ Form.textarea [ class "w-full h-48 mb-2", onInput SetNames ] []
                ]

        submitButton =
            Components.btn [ class "sm:w-1/2 mt-2", disabled model.waitingForResponse, type_ "submit" ] [ text "Create Accounts" ]

        viewForm =
            Html.form [ class "flex flex-col", onSubmit SubmitForm ]
                [ namesInput
                , SelectLevel.view SetLevel model.level
                , submitButton
                ]
    in
    div []
        [ Form.viewErrorMsgs model.errors
        , viewForm
        ]
