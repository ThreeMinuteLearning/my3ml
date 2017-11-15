module Page.Class exposing (Model, Msg, ExternalMsg(..), init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization)
import Dialog
import Dict exposing (Dict)
import Exts.Html.Bootstrap exposing (row)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Table
import Task exposing (Task)
import Util exposing ((=>), dialog, viewIf, defaultHttpErrorMsg)
import Views.Form as Form
import Views.StudentTable as StudentTable


type alias Model =
    { errors : List String
    , class : Api.Class
    , membersTable : Table.State
    , selectedStudents : Dict String Api.Student
    , showConfirmDelete : Bool
    }


type Msg
    = Delete
    | Edit
    | ConfirmDelete
    | DismissDialog
    | DeleteResponse (Result Http.Error Api.NoContent)
    | SetTableState Table.State
    | SelectStudent Api.Student Bool
    | RemoveSelectedStudents
    | ClassMembersResponse (Result Http.Error Api.Class)


type ExternalMsg
    = NoOp
    | Deleted Session
    | Updated Session


init : Session -> String -> Task PageLoadError ( Model, Session )
init session_ slug =
    let
        handleLoadError e =
            pageLoadError e ("Unable to load class data. " ++ defaultHttpErrorMsg e ++ ".")

        loadClass =
            Api.getSchoolClassesByClassId (authorization session_) slug
                |> Http.toTask

        mkModel newSession class =
            ( Model [] class StudentTable.init Dict.empty False, newSession )
    in
        Task.map2 mkModel (Session.loadStudents session_) loadClass
            |> Task.mapError handleLoadError


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        Delete ->
            { model | showConfirmDelete = True } => Cmd.none => NoOp

        DismissDialog ->
            { model | showConfirmDelete = False } => Cmd.none => NoOp

        ConfirmDelete ->
            model
                => (Api.deleteSchoolClassesByClassId (authorization session) (.id model.class)
                        |> Http.send DeleteResponse
                   )
                => NoOp

        DeleteResponse (Ok _) ->
            { model | showConfirmDelete = False }
                => Cmd.none
                => Deleted (deleteFromCache model.class session)

        DeleteResponse (Err _) ->
            model => Cmd.none => NoOp

        Edit ->
            model => Cmd.none => NoOp

        SetTableState state ->
            { model | membersTable = state } => Cmd.none => NoOp

        SelectStudent student checked ->
            let
                f =
                    if checked then
                        Dict.insert student.id student
                    else
                        Dict.remove student.id
            in
                { model | selectedStudents = f model.selectedStudents } => Cmd.none => NoOp

        RemoveSelectedStudents ->
            let
                studentsToDelete =
                    Dict.values model.selectedStudents
                        |> List.map (.id)
            in
                { model | selectedStudents = Dict.empty }
                    => (Api.postSchoolClassesByClassIdMembers (authorization session) (.id model.class) (Just True) studentsToDelete
                            |> Http.send ClassMembersResponse
                       )
                    => NoOp

        ClassMembersResponse (Ok updatedClass) ->
            let
                cache =
                    session.cache

                newClasses =
                    updatedClass
                        :: List.filter (\c -> c.id /= updatedClass.id) cache.classes

                newSession =
                    { session | cache = { cache | classes = newClasses } }
            in
                { model | errors = [], class = updatedClass }
                    => Cmd.none
                    => Updated newSession

        ClassMembersResponse (Err e) ->
            { model | errors = [ defaultHttpErrorMsg e ] }
                => Cmd.none
                => NoOp


deleteFromCache : Api.Class -> Session -> Session
deleteFromCache _ session =
    let
        cache =
            session.cache
    in
        -- Just clear the cache completely for now
        { session | cache = { cache | classes = [] } }


view : Session -> Model -> Html Msg
view session model =
    div [ class "container page" ]
        [ h3 [] [ text (.name model.class) ]
        , h4 [] [ text (Maybe.withDefault "" (.description model.class)) ]
        , viewToolbar model
        , Form.viewErrorMsgs model.errors
        , h4 [] [ text "Class members" ]
        , viewTable session.cache model
        , Dialog.view
            (if model.showConfirmDelete then
                Just (confirmDeleteDialog (userIsOwner session.user model.class))
             else
                Nothing
            )
        ]


viewTable : Session.Cache -> Model -> Html Msg
viewTable cache model =
    let
        tableConfig =
            StudentTable.config SetTableState SelectStudent

        isChecked s =
            Dict.member s.id model.selectedStudents

        classMembers =
            .students model.class

        students =
            List.filter (\s -> List.member s.id classMembers) cache.students
    in
        div [ class "row hidden-print" ]
            [ StudentTable.view tableConfig model.membersTable students isChecked
            ]


userIsOwner : Maybe Session.User -> Api.Class -> Bool
userIsOwner user class =
    case user of
        Nothing ->
            False

        Just u ->
            u.sub == class.createdBy


viewToolbar : Model -> Html Msg
viewToolbar model =
    let
        inputGroupBtn msg txt =
            button [ class "btn btn-default btn-xs", onClick msg, type_ "button" ] [ text txt ]
    in
        row
            [ div [ class "col-lg-6" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-btn" ]
                        [ -- inputGroupBtn Edit "Edit"
                          inputGroupBtn Delete "Delete"
                        ]
                    ]
                ]
            , if Dict.isEmpty model.selectedStudents then
                div [] []
              else
                div [ class "input-group" ]
                    [ span [ class "input-group-btn" ]
                        [ button [ class "btn btn-default", onClick RemoveSelectedStudents, type_ "button" ] [ text "Remove students from class" ]
                        ]
                    ]
            ]


confirmDeleteDialog : Bool -> Dialog.Config Msg
confirmDeleteDialog isOwner =
    dialog DismissDialog
        Nothing
        (div []
            [ p [] [ text "Are you sure you want to delete this class? Only the class information will be removed (none of the student accounts will be affected)." ]
            , viewIf (not isOwner) <|
                p [] [ text "WARNING: Another teacher created this class. Perhaps you shouldn't delete it (they might be annoyed!)?" ]
            , button [ class "btn btn-warn", onClick ConfirmDelete ]
                [ text "Delete class"
                ]
            ]
        )
