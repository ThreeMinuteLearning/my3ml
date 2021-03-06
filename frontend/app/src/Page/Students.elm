module Page.Students exposing (Model, Msg, init, update, view)

import AddStudentsForm
import Api
import Bootstrap
import Cache exposing (Cache)
import Components
import Data.Session as Session exposing (Session, authorization, updateCache)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List.Extra
import Modal
import Page.Errored exposing (PageLoadError, pageLoadError)
import Ports
import Regex
import Route
import Table
import Task exposing (Task)
import Tuple exposing (first, second)
import Util exposing (defaultHttpErrorMsg, maybeView, viewIf)
import Views.ClassSelect as ClassSelect
import Views.Form as Form
import Views.NewAccounts as NewAccounts
import Views.StudentTable as StudentTable
import Views.TeacherToolbar as TeacherToolbar


type Notification
    = Error String
    | Msg String
    | NoMsg


type alias Model =
    { notification : Notification
    , tableState : Table.State
    , selectedStudents : Dict String Api.Student
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
    | AddStudentsFormMsg AddStudentsForm.Msg
    | AddStudentsToClass (Maybe String)
    | ClassMembersResponse (Result Http.Error Api.Class)
    | DismissNotification


init : Session -> Task PageLoadError ( Model, Session )
init session =
    let
        handleLoadError e =
            pageLoadError e ("Unable to load student data. " ++ defaultHttpErrorMsg e ++ ".")

        createModel sesh =
            ( Model NoMsg StudentTable.init Dict.empty Nothing ( "", Nothing ), sesh )
    in
    Session.loadStudents session
        |> Task.andThen (\newSession -> Session.loadClasses newSession)
        |> Task.mapError handleLoadError
        |> Task.map createModel


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Session )
update session msg model =
    case msg of
        ClearSelectedStudents ->
            ( ( { model | selectedStudents = Dict.empty }, Cmd.none ), session )

        ClearNewAccounts ->
            ( ( model, Cmd.none ), updateCache (\c -> { c | newAccounts = [] }) session )

        StudentFilterInput txt ->
            ( ( { model | studentFilter = ( txt, second model.studentFilter ) }, Cmd.none ), session )

        PrintWindow ->
            ( ( model, Ports.printWindow () ), session )

        SetClassFilter c ->
            ( ( { model | studentFilter = ( first model.studentFilter, c ) }, Cmd.none ), session )

        SelectStudent student checked ->
            let
                f =
                    if checked then
                        Dict.insert student.id student

                    else
                        Dict.remove student.id
            in
            ( ( { model | selectedStudents = f model.selectedStudents }, Cmd.none ), session )

        AddStudentsFormMsg subMsg ->
            case Maybe.map (AddStudentsForm.update session subMsg) model.addStudentsForm of
                Nothing ->
                    ( ( model, Cmd.none ), session )

                Just ( ( subModel, subSubMsg ), Nothing ) ->
                    ( ( { model | addStudentsForm = Just subModel }, Cmd.map AddStudentsFormMsg subSubMsg ), session )

                Just ( _, Just newAccounts ) ->
                    let
                        f c =
                            { c | students = List.map first newAccounts ++ c.students, newAccounts = newAccounts ++ c.newAccounts }
                    in
                    ( ( { model | addStudentsForm = Nothing }, Cmd.none ), updateCache f session )

        SetTableState state ->
            ( ( { model | tableState = state }, Cmd.none ), session )

        ShowAddStudents ->
            ( ( { model | addStudentsForm = Just AddStudentsForm.init }, Cmd.none ), session )

        DismissAddStudents ->
            ( ( { model | addStudentsForm = Nothing }, Cmd.none ), session )

        AddStudentsToClass classId ->
            case classId of
                Nothing ->
                    ( ( model, Cmd.none ), session )

                Just cid ->
                    let
                        studentsToAdd =
                            Dict.values model.selectedStudents
                                |> List.map .id
                    in
                    ( ( { model | selectedStudents = Dict.empty }
                      , Api.postSchoolClassesByClassIdMembers (authorization session) cid Nothing studentsToAdd
                            |> Http.send ClassMembersResponse
                      )
                    , session
                    )

        ClassMembersResponse (Ok updatedClass) ->
            let
                updateClasses cs =
                    updatedClass
                        :: List.filter (\c -> c.id /= updatedClass.id) cs

                newSession =
                    updateCache (\c -> { c | classes = updateClasses c.classes }) session
            in
            ( ( { model | notification = Msg "Class members updated" }, Cmd.none ), newSession )

        ClassMembersResponse (Err e) ->
            ( ( { model | notification = Error ("Could't update add class members: " ++ defaultHttpErrorMsg e) }, Cmd.none ), session )

        DismissNotification ->
            ( ( clearNotification model, Cmd.none ), session )


clearNotification : Model -> Model
clearNotification m =
    { m | notification = NoMsg }


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    let
        cache =
            Session.getCache session
    in
    { title = "Students"
    , content =
        div [ class "flex flex-col" ]
            [ div [ class "mb-4" ] [ TeacherToolbar.view session Route.Students (subtools session) ]
            , viewNotification model.notification
            , div [ class "mb-4" ] [ NewAccounts.view PrintWindow ClearNewAccounts cache.newAccounts ]
            , div [ class "print:none flex flex-col" ]
                [ div [ class "flex mb-4" ]
                    [ Form.input
                        [ class "mr-1"
                        , type_ "text"
                        , value (first model.studentFilter)
                        , onInput StudentFilterInput
                        , placeholder "Name search"
                        , id "studentNameFilter"
                        ]
                        []
                    , ClassSelect.view cache.classes (second model.studentFilter) "Filter by class" SetClassFilter
                    ]
                , Util.viewUnless (Dict.isEmpty model.selectedStudents)
                    (Components.toolbar [ ( ClearSelectedStudents, False, "Clear selection" ) ] [ ClassSelect.view cache.classes Nothing "Add selected students to class" AddStudentsToClass ])
                ]
            , viewTable cache model
            , maybeView addStudentsDialog model.addStudentsForm
            ]
    }


subtools : Session -> List (Html Msg)
subtools session =
    if Session.isSchoolAdmin session then
        [ Components.btn [ id "add-students-button", onClick ShowAddStudents ] [ text "Add Students" ]
        ]

    else
        []


viewNotification : Notification -> Html Msg
viewNotification n =
    case n of
        NoMsg ->
            text ""

        Error msg ->
            div [ class "my-4" ] [ Bootstrap.alert Bootstrap.Danger msg DismissNotification ]

        Msg msg ->
            div [ class "my-4" ] [ Bootstrap.alert Bootstrap.Success msg DismissNotification ]


viewTable : Cache -> Model -> Html Msg
viewTable cache model =
    let
        elements =
            filterStudents cache model

        tableConfig =
            StudentTable.config SetTableState SelectStudent

        isChecked s =
            Dict.member s.id model.selectedStudents
    in
    div [ class "print:none" ]
        [ StudentTable.view tableConfig model.tableState elements isChecked
        ]


filterStudents : Cache -> Model -> List Api.Student
filterStudents cache model =
    case model.studentFilter of
        ( _, Just classId ) ->
            findStudentsInClass cache classId
                |> Maybe.map (filterByStudentIds cache.students)
                |> Maybe.withDefault []

        ( nameFilter, Nothing ) ->
            if String.length nameFilter < 3 then
                cache.students

            else
                filterStudentsByName nameFilter cache.students


findStudentsInClass : Cache -> String -> Maybe (List String)
findStudentsInClass cache classId =
    List.Extra.find (\c -> c.id == classId) cache.classes
        |> Maybe.map .students


filterByStudentIds : List Api.Student -> List String -> List Api.Student
filterByStudentIds students ids =
    List.filter (\s -> List.member s.id ids) students


filterStudentsByName : String -> List Api.Student -> List Api.Student
filterStudentsByName nameFilter students =
    Regex.fromStringWith { caseInsensitive = True, multiline = False } nameFilter
        |> Maybe.withDefault Regex.never
        |> (\r -> List.filter (\s -> Regex.contains r s.name) students)


addStudentsDialog : AddStudentsForm.Model -> Html Msg
addStudentsDialog form =
    Modal.view "Add Students"
        DismissAddStudents
        (div [ class "w-full max-w-xl p-4 flex flex-col" ]
            [ p [ class "mb-2 text-lg" ] [ text "Enter the names of the students you want to add accounts for, separated by commas or on separate lines." ]
            , p [ class "mb-2 text-lg" ] [ text "You can enter up to 100 names." ]
            , Html.map AddStudentsFormMsg (AddStudentsForm.view form)
            ]
        )
