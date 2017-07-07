module Page.Student exposing (Model, Msg, init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Dialog
import Exts.Html.Bootstrap exposing (row)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Util exposing ((=>), dialog)
import Views.Answers as Answers
import Views.ChangePasswordForm as ChangePassword
import Views.ChangeUsernameForm as ChangeUsername
import Views.SelectLevel as SelectLevel


type alias Model =
    { student : Api.Student
    , answers : List ( Api.Answer, Api.Story )
    , changePasswordForm : Maybe ChangePassword.Model
    , changeUsernameForm : Maybe ChangeUsername.Model
    , showConfirmDelete : Bool
    , showTimer : Bool
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
            pageLoadError e "Unable to load data for page."

        loadStudent =
            Api.getSchoolStudentsByStudentId (authorization session_) slug
                |> Http.toTask

        loadAnswers =
            Api.getSchoolAnswers (authorization session_) Nothing (Just slug)
                |> Http.toTask

        zipWithStory session a =
            Maybe.map ((,) a) (findStoryById session.cache a.storyId)

        mkModel newSession student answers =
            ( Model student (List.filterMap (zipWithStory newSession) answers) Nothing Nothing False False, newSession )
    in
        Task.map3 mkModel (Session.loadStories session_) loadStudent loadAnswers
            |> Task.mapError handleLoadError


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        ShowChangePassword ->
            { model | changePasswordForm = Just <| ChangePassword.init (.id model.student) 8 }
                => Cmd.none

        ChangePasswordMsg subMsg ->
            case Maybe.map (ChangePassword.update session subMsg) model.changePasswordForm of
                Nothing ->
                    ( model, Cmd.none )

                Just ( ( subModel, subSubMsg ), ChangePassword.NoOp ) ->
                    { model | changePasswordForm = Just subModel }
                        => Cmd.map ChangePasswordMsg subSubMsg

                Just ( _, ChangePassword.Completed ) ->
                    { model | changePasswordForm = Nothing }
                        => Cmd.none

        ShowChangeUsername ->
            { model | changeUsernameForm = Just <| ChangeUsername.init (.id model.student) }
                => Cmd.none

        ChangeUsernameMsg subMsg ->
            case Maybe.map (ChangeUsername.update session subMsg) model.changeUsernameForm of
                Nothing ->
                    ( model, Cmd.none )

                Just ( ( subModel, subSubMsg ), ChangeUsername.NoOp ) ->
                    { model | changeUsernameForm = Just subModel }
                        => Cmd.map ChangeUsernameMsg subSubMsg

                Just ( _, ChangeUsername.Completed ) ->
                    { model | changeUsernameForm = Nothing }
                        => Cmd.none

        ToggleHiddenStatus ->
            let
                s =
                    model.student

                newStudent =
                    { s | hidden = not s.hidden }
            in
                model => sendUpdateStudent session newStudent

        ToggleDeletedStatus ->
            case .deleted model.student of
                Nothing ->
                    { model | showConfirmDelete = True } => Cmd.none

                Just _ ->
                    let
                        s =
                            model.student
                    in
                        model
                            => (Api.postSchoolStudentsByStudentIdUndelete (authorization session) (.id model.student)
                                    |> Http.send UndeleteResponse
                               )

        ConfirmDelete ->
            model
                => (Api.deleteSchoolStudentsByStudentId (authorization session) (.id model.student)
                        |> Http.send UpdateStudentResponse
                   )

        UndeleteResponse (Ok _) ->
            let
                student =
                    model.student
            in
                { model | student = { student | deleted = Nothing } } => Cmd.none

        UndeleteResponse (Err _) ->
            model => Cmd.none

        UpdateStudentResponse (Ok student) ->
            { model | student = student, showConfirmDelete = False } => Cmd.none

        UpdateStudentResponse (Err _) ->
            model => Cmd.none

        SetLevel newLevel ->
            if newLevel == (.level model.student) then
                model => Cmd.none
            else
                let
                    s =
                        model.student
                in
                    model
                        => sendUpdateStudent session { s | level = newLevel }

        DismissDialog ->
            { model | changeUsernameForm = Nothing, changePasswordForm = Nothing, showConfirmDelete = False }
                => Cmd.none


sendUpdateStudent : Session -> Api.Student -> Cmd Msg
sendUpdateStudent session student =
    Api.postSchoolStudentsByStudentId (authorization session) student.id student
        |> Http.send UpdateStudentResponse


view : Model -> Html Msg
view model =
    div [ class "container page" ]
        [ h3 [] [ text (.name model.student) ]
        , viewToolbar model.student
        , Answers.viewWithStories model.answers
        , Dialog.view (Maybe.map changePasswordDialog model.changePasswordForm)
        , Dialog.view (Maybe.map changeUsernameDialog model.changeUsernameForm)
        , Dialog.view
            (if model.showConfirmDelete then
                Just confirmDeleteDialog
             else
                Nothing
            )
        ]


viewToolbar : Api.Student -> Html Msg
viewToolbar student =
    let
        inputGroupBtn msg txt =
            button [ class "btn btn-default", onClick msg, type_ "button" ] [ text txt ]
    in
        row
            [ div [ class "col-lg-8" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-btn" ]
                        [ inputGroupBtn ShowChangePassword "Change password"
                        , inputGroupBtn ShowChangeUsername "Change username"
                        , inputGroupBtn ToggleDeletedStatus
                            (case student.deleted of
                                Nothing ->
                                    "Delete"

                                _ ->
                                    "Un-delete"
                            )
                        , inputGroupBtn ToggleHiddenStatus
                            (if student.hidden then
                                "Un-hide"
                             else
                                "Hide"
                            )
                        ]
                    , SelectLevel.view SetLevel student.level
                    ]
                ]
            ]


changePasswordDialog : ChangePassword.Model -> Dialog.Config Msg
changePasswordDialog form =
    dialog
        DismissDialog
        (Just (h3 [] [ text "Change password" ]))
        (div []
            [ ChangePassword.view form
                |> Html.map ChangePasswordMsg
            ]
        )


changeUsernameDialog : ChangeUsername.Model -> Dialog.Config Msg
changeUsernameDialog form =
    dialog
        DismissDialog
        (Just (h3 [] [ text "Change username" ]))
        (div []
            [ ChangeUsername.view form
                |> Html.map ChangeUsernameMsg
            ]
        )


confirmDeleteDialog : Dialog.Config Msg
confirmDeleteDialog =
    dialog DismissDialog
        Nothing
        (div []
            [ p [] [ text "Are you sure you want to delete this student account? It will be marked for deletion and removed automatically at a later date (you can un-delete it if you change your mind)." ]
            , button [ class "btn btn-danger", onClick ConfirmDelete ]
                [ text "Delete student"
                ]
            ]
        )
