module Page.Student exposing (Model, Msg, init, update, view)

import Api
import Components
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Modal
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Tuple exposing (pair)
import Util exposing (defaultHttpErrorMsg, maybeView, viewIf)
import Views.Answers as Answers
import Views.ChangePasswordForm as ChangePassword
import Views.ChangeUsernameForm as ChangeUsername
import Views.Form as Form
import Views.SelectLevel as SelectLevel


type alias Model =
    { errors : List String
    , student : Api.Student
    , answers : List ( Api.Answer, Api.Story )
    , changePasswordForm : Maybe ChangePassword.Model
    , changeUsernameForm : Maybe ChangeUsername.Model
    , showConfirmDelete : Bool
    , showTimer : Bool
    , userIsAdmin : Bool
    }


type Msg
    = ShowChangePassword
    | ChangePasswordMsg ChangePassword.Msg
    | ShowChangeUsername
    | ChangeUsernameMsg ChangeUsername.Msg
    | ToggleHiddenStatus
    | ToggleDeletedStatus
    | ConfirmDelete
    | UpdateStudentResponse (Result Http.Error Api.Student)
    | UndeleteResponse (Result Http.Error Api.NoContent)
    | SetLevel Int
    | DismissDialog


init : Session -> String -> Task PageLoadError ( Model, Session )
init session_ slug =
    let
        handleLoadError e =
            pageLoadError e (defaultHttpErrorMsg e)

        loadStudent =
            Api.getSchoolStudentsByStudentId (authorization session_) slug
                |> Http.toTask

        loadAnswers =
            Api.getSchoolAnswers (authorization session_) Nothing (Just slug)
                |> Http.toTask

        zipWithStory cache a =
            Maybe.map (pair a) (findStoryById cache a.storyId)

        mkModel newSession student answers =
            ( Model [] student (List.filterMap (zipWithStory (Session.getCache newSession)) answers) Nothing Nothing False False (Session.isSchoolAdmin newSession), newSession )
    in
    Task.map3 mkModel (Session.loadStories session_) loadStudent loadAnswers
        |> Task.mapError handleLoadError


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Session )
update session msg model =
    case msg of
        ShowChangePassword ->
            ( ( { model | changePasswordForm = Just <| ChangePassword.init (.id model.student) 8 }, Cmd.none ), session )

        ChangePasswordMsg subMsg ->
            case Maybe.map (ChangePassword.update session subMsg) model.changePasswordForm of
                Nothing ->
                    ( ( model, Cmd.none ), session )

                Just ( ( subModel, subSubMsg ), ChangePassword.NoOp ) ->
                    ( ( { model | changePasswordForm = Just subModel }, Cmd.map ChangePasswordMsg subSubMsg ), session )

                Just ( _, ChangePassword.Completed ) ->
                    ( ( { model | changePasswordForm = Nothing }, Cmd.none ), session )

        ShowChangeUsername ->
            ( ( { model | changeUsernameForm = Just <| ChangeUsername.init (.id model.student) }, Cmd.none ), session )

        ChangeUsernameMsg subMsg ->
            case Maybe.map (ChangeUsername.update session subMsg) model.changeUsernameForm of
                Nothing ->
                    ( ( model, Cmd.none ), session )

                Just ( ( subModel, subSubMsg ), ChangeUsername.NoOp ) ->
                    ( ( { model | changeUsernameForm = Just subModel }, Cmd.map ChangeUsernameMsg subSubMsg ), session )

                Just ( _, ChangeUsername.Completed ) ->
                    ( ( { model | changeUsernameForm = Nothing }, Cmd.none ), session )

        ToggleHiddenStatus ->
            let
                s =
                    model.student

                newStudent =
                    { s | hidden = not s.hidden }
            in
            ( ( model, sendUpdateStudent session newStudent ), session )

        ToggleDeletedStatus ->
            case .deleted model.student of
                Nothing ->
                    ( ( { model | showConfirmDelete = True }, Cmd.none ), session )

                Just _ ->
                    let
                        s =
                            model.student
                    in
                    ( ( model
                      , Api.postSchoolStudentsByStudentIdUndelete (authorization session) (.id model.student)
                            |> Http.send UndeleteResponse
                      )
                    , session
                    )

        ConfirmDelete ->
            ( ( model
              , Api.deleteSchoolStudentsByStudentId (authorization session) (.id model.student)
                    |> Http.send UpdateStudentResponse
              )
            , session
            )

        UndeleteResponse (Ok _) ->
            let
                student =
                    model.student
            in
            ( ( { model | errors = [], student = { student | deleted = Nothing } }, Cmd.none ), updateSession session student )

        UndeleteResponse (Err e) ->
            ( ( { model | errors = [ defaultHttpErrorMsg e ] }, Cmd.none ), session )

        UpdateStudentResponse (Ok student) ->
            ( ( { model | errors = [], student = student, showConfirmDelete = False }, Cmd.none ), updateSession session student )

        UpdateStudentResponse (Err e) ->
            ( ( { model | errors = [ defaultHttpErrorMsg e ] }, Cmd.none ), session )

        SetLevel newLevel ->
            if newLevel == .level model.student then
                ( ( model, Cmd.none ), session )

            else
                let
                    s =
                        model.student
                in
                ( ( model, sendUpdateStudent session { s | level = newLevel } ), session )

        DismissDialog ->
            ( ( { model | changeUsernameForm = Nothing, changePasswordForm = Nothing, showConfirmDelete = False }, Cmd.none ), session )


updateSession : Session -> Api.Student -> Session
updateSession session student =
    let
        cache =
            Session.getCache session

        newStudents =
            student :: List.filter (\s -> s.id /= student.id) cache.students
    in
    Session.updateCache (\c -> { c | students = newStudents }) session


sendUpdateStudent : Session -> Api.Student -> Cmd Msg
sendUpdateStudent session student =
    Api.postSchoolStudentsByStudentId (authorization session) student.id student
        |> Http.send UpdateStudentResponse


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Student"
    , content =
        div []
            [ h1 [ class "font-normal text-lg text-gray-700" ] [ text (.name model.student) ]
            , viewToolbar model.userIsAdmin model.student
            , Form.viewErrorMsgs model.errors
            , h2 [ class "font-light text-lg text-gray-600 mt-4" ] [ text "Completed Stories" ]
            , Answers.viewWithStories model.answers
            , maybeView changePasswordDialog model.changePasswordForm
            , maybeView changeUsernameDialog model.changeUsernameForm
            , viewIf model.showConfirmDelete confirmDeleteDialog
            ]
    }


viewToolbar : Bool -> Api.Student -> Html Msg
viewToolbar isAdmin student =
    let
        buttons =
            ( ShowChangePassword, False, "Change password" )
                :: ( ShowChangeUsername, False, "Change username" )
                :: (if isAdmin then
                        [ ( ToggleDeletedStatus
                          , False
                          , case student.deleted of
                                Nothing ->
                                    "Delete"

                                _ ->
                                    "Un-delete"
                          )
                        , ( ToggleHiddenStatus
                          , student.deleted /= Nothing
                          , if student.hidden then
                                "Un-hide"

                            else
                                "Hide"
                          )
                        ]

                    else
                        []
                   )
    in
    Components.toolbar buttons [ SelectLevel.view SetLevel student.level ]


changePasswordDialog : ChangePassword.Model -> Html Msg
changePasswordDialog form =
    Modal.view "Change password"
        DismissDialog
        (div []
            [ Html.map ChangePasswordMsg (ChangePassword.view form)
            ]
        )


changeUsernameDialog : ChangeUsername.Model -> Html Msg
changeUsernameDialog form =
    Modal.view "Change username"
        DismissDialog
        (div [ class "w-full max-w-sm p-4 flex flex-col" ]
            [ Html.map ChangeUsernameMsg (ChangeUsername.view form)
            ]
        )


confirmDeleteDialog : Html Msg
confirmDeleteDialog =
    Modal.view "Delete Student"
        DismissDialog
        (div [ class "w-full max-w-xl p-4 flex flex-col" ]
            [ p [ class "text-lg mb-4" ] [ text "Are you sure you want to delete this student account?" ]
            , p [ class "text-lg" ] [ text "It will be marked for deletion and removed automatically at a later date (you can un-delete it if you change your mind)." ]
            , Components.btn [ class "mt-6", onClick ConfirmDelete ] [ text "Delete student" ]
            ]
        )
