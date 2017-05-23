module Page.Students exposing (Model, Msg, init, update, view)

import AddStudentsForm
import Api
import Bootstrap
import Data.Session as Session exposing (Session, authorization)
import Dialog
import Dict exposing (Dict)
import Exts.Html.Bootstrap exposing (formGroup, row)
import Exts.List exposing (firstMatch)
import Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Ports
import Regex
import Table
import Task exposing (Task)
import Tuple exposing (first, second)
import Tuple exposing (second)
import Util exposing ((=>))
import Views.NewAccounts as NewAccounts
import Views.Page as Page
import Views.TeacherToolbar as TeacherToolbar


type alias Model =
    { students : List Api.Student
    , classes : List Api.Class
    , tableState : Table.State
    , selectedStudents : Dict String Api.Student
    , studentAccountsCreated : List ( Api.Student, ( String, String ) )
    , addStudentsForm : Maybe AddStudentsForm.Model
    , studentFilter : ( String, Maybe String )
    }


type Msg
    = ClearSelectedStudents
    | ClearNewAccounts
    | PrintWindow
    | ShowAddStudents
    | DismissAddStudents
    | StudentFilterInput String
    | SetClassFilter (Maybe String)
    | SetTableState Table.State
    | SelectStudent Api.Student Bool
    | AddStudentsFormMsg Form.Msg
    | AddStudentsResponse (Result Http.Error (List ( Api.Student, ( String, String ) )))


init : Session -> Task PageLoadError ( Model, Session )
init session =
    let
        handleLoadError _ =
            pageLoadError Page.Other "Unable to load student data."

        createModel session =
            Model (.students session.cache) (.classes session.cache) (Table.initialSort "Name") Dict.empty [] Nothing ( "", Nothing )
                => session
    in
        Session.loadStudents session
            |> Task.andThen (\newSession -> Session.loadClasses newSession)
            |> Task.mapError handleLoadError
            |> Task.map createModel


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        ClearSelectedStudents ->
            { model | selectedStudents = Dict.empty } => Cmd.none

        ClearNewAccounts ->
            { model | studentAccountsCreated = [] } => Cmd.none

        StudentFilterInput txt ->
            { model | studentFilter = ( txt, second model.studentFilter ) } => Cmd.none

        PrintWindow ->
            model => Ports.printWindow ()

        SetClassFilter c ->
            { model | studentFilter = ( first model.studentFilter, c ) } => Cmd.none

        SelectStudent student checked ->
            let
                f =
                    if checked then
                        Dict.insert student.id student
                    else
                        Dict.remove student.id
            in
                { model | selectedStudents = f model.selectedStudents } => Cmd.none

        AddStudentsFormMsg subMsg ->
            case Maybe.map (AddStudentsForm.update subMsg) model.addStudentsForm of
                Nothing ->
                    ( model, Cmd.none )

                Just ( subModel, Nothing ) ->
                    { model | addStudentsForm = Just subModel } => Cmd.none

                Just ( _, Just names ) ->
                    { model | addStudentsForm = Nothing }
                        => (Api.postSchoolStudents (authorization session) names
                                |> Http.send AddStudentsResponse
                           )

        SetTableState state ->
            { model | tableState = state } => Cmd.none

        AddStudentsResponse (Ok newAccounts) ->
            let
                accountsCreated =
                    newAccounts ++ model.studentAccountsCreated

                newStudents =
                    List.map first newAccounts
            in
                { model | studentAccountsCreated = accountsCreated, students = List.append newStudents model.students } => Cmd.none

        ShowAddStudents ->
            { model | addStudentsForm = Just AddStudentsForm.init } => Cmd.none

        DismissAddStudents ->
            { model | addStudentsForm = Nothing } => Cmd.none

        AddStudentsResponse (Err _) ->
            model => Cmd.none


view : Model -> Html Msg
view model =
    div [ class "container page" ]
        [ TeacherToolbar.view subtools
        , viewTable model
        , Dialog.view (Maybe.map addStudentsDialog model.addStudentsForm)
        ]


subtools : List (Html Msg)
subtools =
    [ Bootstrap.btn ShowAddStudents [ text "Add Students" ] ]


viewTable : Model -> Html Msg
viewTable model =
    let
        elements =
            filterStudents model model.students
                |> List.map (\s -> ( Dict.member s.id model.selectedStudents, s ))
    in
        div []
            [ row [ NewAccounts.view PrintWindow ClearNewAccounts model.studentAccountsCreated ]
            , div [ class "row hidden-print" ] [ viewStudentsFilter model ]
            , div [ class "row hidden-print" ]
                [ Table.view tableConfig model.tableState elements
                ]
            ]


tableConfig : Table.Config ( Bool, Api.Student ) Msg
tableConfig =
    Table.customConfig
        { toId = .id << second
        , toMsg = SetTableState
        , columns =
            [ checkboxColumn
            , Table.stringColumn "Name" (.name << second)
            , Table.intColumn "Level" (.level << second)
            ]
        , customizations = Bootstrap.tableCustomizations
        }


checkboxColumn : Table.Column ( Bool, Api.Student ) Msg
checkboxColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = viewCheckbox
        , sorter = Table.unsortable
        }


viewCheckbox : ( Bool, Api.Student ) -> Table.HtmlDetails Msg
viewCheckbox ( selected, s ) =
    Table.HtmlDetails []
        [ input [ type_ "checkbox", onCheck (SelectStudent s), checked selected ] []
        ]


filterStudents : Model -> List Api.Student -> List Api.Student
filterStudents sd students =
    case sd.studentFilter of
        ( _, Just classId ) ->
            findStudentsInClass sd classId
                |> Maybe.map (filterByStudentIds students)
                |> Maybe.withDefault []

        ( nameFilter, Nothing ) ->
            if String.length nameFilter < 3 then
                students
            else
                filterStudentsByName nameFilter students


findStudentsInClass : Model -> String -> Maybe (List String)
findStudentsInClass model classId =
    firstMatch (\c -> c.id == classId) model.classes
        |> Maybe.map .students


filterByStudentIds : List Api.Student -> List String -> List Api.Student
filterByStudentIds students ids =
    List.filter (\s -> List.member s.id ids) students


filterStudentsByName : String -> List Api.Student -> List Api.Student
filterStudentsByName nameFilter students =
    Regex.caseInsensitive (Regex.regex nameFilter)
        |> \r -> List.filter (\s -> Regex.contains r s.name) students


viewStudentsFilter : Model -> Html Msg
viewStudentsFilter model =
    let
        selectedClass =
            Maybe.withDefault "" (second model.studentFilter)

        format description =
            description
                |> Maybe.map (\d -> " (" ++ d ++ ")")
                |> Maybe.withDefault ""

        classOption c =
            Html.option
                [ selected (selectedClass == c.id)
                , value c.id
                ]
                [ text (c.name ++ format c.description) ]

        emptyOption =
            Html.option [ value "" ] [ text "" ]

        onSelect classId =
            SetClassFilter <|
                if classId == "" then
                    Nothing
                else
                    (Just classId)
    in
        formGroup
            [ label [ for "studentNameFilter" ] [ text "Search by name" ]
            , input
                [ type_ "text"
                , value (first model.studentFilter)
                , onInput StudentFilterInput
                , id "studentNameFilter"
                ]
                []
            , label [ for "studentClass" ] [ text "Filter by class" ]
            , Html.select [ onInput (\s -> onSelect s) ]
                (emptyOption :: List.map classOption model.classes)
            , Bootstrap.btn ClearSelectedStudents [ text "Clear Selection" ]
            ]


addStudentsDialog : AddStudentsForm.Model -> Dialog.Config Msg
addStudentsDialog form =
    { closeMessage = Just DismissAddStudents
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Add Students" ])
    , body =
        Just <|
            div []
                [ p [] [ text "Enter the names of up to ten students you want to add accounts for (one student per input field)." ]
                , AddStudentsForm.view form
                    |> Html.map AddStudentsFormMsg
                ]
    , footer = Nothing
    }
