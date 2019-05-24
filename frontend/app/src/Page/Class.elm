module Page.Class exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Api
import Cache exposing (Cache)
import Components
import Data.Session as Session exposing (Session, authorization)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Modal
import Page.Errored exposing (PageLoadError, pageLoadError)
import Route
import Table
import Task exposing (Task)
import Util exposing (defaultHttpErrorMsg, viewIf)
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
            ( ( { model | showConfirmDelete = True }, Cmd.none ), NoOp )

        DismissDialog ->
            ( ( { model | showConfirmDelete = False }, Cmd.none ), NoOp )

        ConfirmDelete ->
            ( ( model
              , Api.deleteSchoolClassesByClassId (authorization session) (.id model.class)
                    |> Http.send DeleteResponse
              )
            , NoOp
            )

        DeleteResponse (Ok _) ->
            ( ( { model | showConfirmDelete = False }
              , Cmd.none
              )
            , Deleted (deleteFromCache model.class session)
            )

        DeleteResponse (Err _) ->
            ( ( model, Cmd.none ), NoOp )

        Edit ->
            ( ( model, Cmd.none ), NoOp )

        SetTableState state ->
            ( ( { model | membersTable = state }, Cmd.none ), NoOp )

        SelectStudent student checked ->
            let
                f =
                    if checked then
                        Dict.insert student.id student

                    else
                        Dict.remove student.id
            in
            ( ( { model | selectedStudents = f model.selectedStudents }, Cmd.none ), NoOp )

        RemoveSelectedStudents ->
            let
                studentsToDelete =
                    Dict.values model.selectedStudents
                        |> List.map .id
            in
            ( ( { model | selectedStudents = Dict.empty }
              , Api.postSchoolClassesByClassIdMembers (authorization session) (.id model.class) (Just True) studentsToDelete
                    |> Http.send ClassMembersResponse
              )
            , NoOp
            )

        ClassMembersResponse (Ok updatedClass) ->
            let
                updateClasses cs =
                    updatedClass
                        :: List.filter (\c -> c.id /= updatedClass.id) cs

                newSession =
                    Session.updateCache (\c -> { c | classes = updateClasses c.classes }) session
            in
            ( ( { model | errors = [], class = updatedClass }, Cmd.none ), Updated newSession )

        ClassMembersResponse (Err e) ->
            ( ( { model | errors = [ defaultHttpErrorMsg e ] }, Cmd.none ), NoOp )


deleteFromCache : Api.Class -> Session -> Session
deleteFromCache _ session =
    -- Just clear the cache completely for now
    Session.updateCache (\c -> { c | classes = [] }) session


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title = "Class " ++ model.class.name
    , content =
        div []
            [ h1 [ class "font-normal text-lg mb-2" ] [ text (.name model.class) ]
            , div [ class "text-sm text-gray-600 font-semi-bold mb-1" ] [ text (Maybe.withDefault "" (.description model.class)) ]
            , viewToolbar model
            , Form.viewErrorMsgs model.errors
            , h2 [ class "text-lg font-light mt-2" ] [ text "Class members" ]
            , if List.isEmpty model.class.students then
                div [ class "mt-2 text-base" ]
                    [ p [ class "mb-1" ]
                        [ text "This class is empty." ]
                    , p []
                        [ text "You can select students in the table on the "
                        , Components.link [ Route.href (Route.Teacher Route.Students) ] "students page"
                        , text " to add new members."
                        ]
                    ]

              else
                viewTable (Session.getCache session) model
            , viewIf model.showConfirmDelete
                (confirmDeleteDialog (userIsOwner session model.class))
            ]
    }


viewTable : Cache -> Model -> Html Msg
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
    div [ class "print:none" ]
        [ StudentTable.view tableConfig model.membersTable students isChecked
        ]


userIsOwner : Session -> Api.Class -> Bool
userIsOwner session class =
    case Session.subjectId session of
        Nothing ->
            False

        Just subId ->
            subId == class.createdBy


viewToolbar : Model -> Html Msg
viewToolbar model =
    let
        buttons =
            ( Delete, False, "Delete" )
                :: (if not (Dict.isEmpty model.selectedStudents) then
                        [ ( RemoveSelectedStudents, False, "Remove students from class" ) ]

                    else
                        []
                   )
    in
    Components.toolbar buttons []


confirmDeleteDialog : Bool -> Html Msg
confirmDeleteDialog isOwner =
    Modal.view "Delete class"
        DismissDialog
        (div [ class "w-full max-w-xl p-4 flex flex-col" ]
            [ p [ class "text-lg mb-4" ] [ text "Are you sure you want to delete this class?" ]
            , p [ class "text-lg mb-4" ] [ text "Only the class information will be removed (none of the student accounts will be affected)." ]
            , viewIf (not isOwner) <|
                p [ class "text-lg font-bold" ] [ text "WARNING: Another teacher created this class. Perhaps you shouldn't delete it (they might be annoyed!)?" ]
            , Components.btn [ class "mt-4", onClick ConfirmDelete ] [ text "Delete class" ]
            ]
        )
