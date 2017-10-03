module Page.Teachers exposing (Model, Msg, init, update, view)

import Api
import Bootstrap
import Data.Session as Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Table
import Task exposing (Task)
import Tuple exposing (first, second)
import Util exposing ((=>), defaultHttpErrorMsg)
import Views.TeacherToolbar as TeacherToolbar


type alias Model =
    { tableState : Table.State
    , registrationCode : Maybe String
    , teachers : List ( Api.Teacher, Bool )
    }


type Msg
    = SetTableState Table.State
    | ActivateAccount String
    | ActivateAccountResponse (Result Http.Error String)
    | GenerateRegistrationCode
    | GenerateRegistrationCodeResponse (Result Http.Error String)


init : Session -> Task PageLoadError Model
init session =
    let
        handleLoadError e =
            pageLoadError e ("Unable to load teacher data. " ++ defaultHttpErrorMsg e ++ ".")

        createModel =
            Model (Table.initialSort "Name") Nothing
    in
        Api.getSchoolTeachers (authorization session)
            |> Http.toTask
            |> Task.mapError handleLoadError
            |> Task.map createModel


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SetTableState state ->
            { model | tableState = state } => Cmd.none

        ActivateAccount accId ->
            model
                => (Api.postSchoolTeachersByTeacherIdActivate (authorization session) accId
                        |> Http.send ActivateAccountResponse
                   )

        ActivateAccountResponse (Ok accId) ->
            { model | teachers = List.map (markActivated accId) model.teachers } => Cmd.none

        ActivateAccountResponse (Err e) ->
            model => Cmd.none

        GenerateRegistrationCode ->
            model
                => (Api.getAccountRegisterCode (authorization session)
                        |> Http.send GenerateRegistrationCodeResponse
                   )

        GenerateRegistrationCodeResponse (Ok code) ->
            { model | registrationCode = Just code } => Cmd.none

        GenerateRegistrationCodeResponse (Err _) ->
            model => Cmd.none


markActivated : String -> ( Api.Teacher, Bool ) -> ( Api.Teacher, Bool )
markActivated accId teacher =
    if .id (first teacher) == accId then
        ( first teacher, True )
    else
        teacher


view : Session -> Model -> Html Msg
view session model =
    div [ class "container page" ]
        [ TeacherToolbar.view session [ newRegistrationCodeButton ]
        , viewCode model.registrationCode
        , viewTable model
        ]


viewTable : Model -> Html Msg
viewTable model =
    div [ class "row hidden-print" ]
        [ Table.view tableConfig model.tableState model.teachers
        ]


viewCode : Maybe String -> Html msg
viewCode code =
    case code of
        Nothing ->
            div [] []

        Just c ->
            div [ class "row" ]
                [ p [] [ text "Copy the code below and give it to the person you want to create an account for. They should then register for an account and enter the code to become a member of your school. The code is valid for 20 minutes." ]
                , p [] [ text "Once they have completed their registration, they should tell you and you can activate their account from this page (reload the page if necessary)." ]
                , div [ class "registration-code text-center" ]
                    [ p [] [ text c ]
                    ]
                ]


newRegistrationCodeButton : Html Msg
newRegistrationCodeButton =
    Bootstrap.btn GenerateRegistrationCode [ text "New Teacher Account" ]


tableConfig : Table.Config ( Api.Teacher, Bool ) Msg
tableConfig =
    Table.customConfig
        { toId = .id << first
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Name" (.name << first)
            , Table.veryCustomColumn
                { name = ""
                , viewData = viewActivationButton
                , sorter = Table.unsortable
                }
            ]
        , customizations = Bootstrap.tableCustomizations
        }


viewActivationButton : ( Api.Teacher, Bool ) -> Table.HtmlDetails Msg
viewActivationButton ( teacher, isActive ) =
    if isActive then
        Table.HtmlDetails [] []
    else
        Table.HtmlDetails []
            [ button [ class "btn btn-default", type_ "button", onClick (ActivateAccount teacher.id) ] [ text "Activate account" ]
            ]
