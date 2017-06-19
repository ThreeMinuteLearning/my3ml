module Page.Student exposing (Model, Msg, init, update, view)

import Api
import Bootstrap exposing (toolbar, btnGroup)
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Dialog
import Exts.Html.Bootstrap exposing (row)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Util exposing ((=>))
import Views.ChangePasswordForm as ChangePassword
import Views.ChangeUsernameForm as ChangeUsername
import Views.Page as Page


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
    | DismissChangePassword
    | ChangePasswordMsg ChangePassword.Msg
    | ShowChangeUsername
    | DismissChangeUsername
    | ChangeUsernameMsg ChangeUsername.Msg
    | ToggleHiddenStatus
    | ToggleDeletedStatus
    | ConfirmDelete
    | DismissConfirmDelete
    | UpdateStudentResponse (Result Http.Error Api.Student)
    | UndeleteResponse (Result Http.Error Api.NoContent)


init : Session -> String -> Task PageLoadError ( Model, Session )
init session slug =
    let
        handleLoadError _ =
            pageLoadError Page.Other "Unable to load data for page."

        loadStudent =
            Api.getSchoolStudentsByStudentId (authorization session) slug
                |> Http.toTask

        loadAnswers =
            Api.getSchoolAnswers (authorization session) Nothing (Just slug)
                |> Http.toTask

        zipWithStory a =
            Maybe.map ((,) a) (findStoryById session.cache a.storyId)

        mkModel newSession student answers =
            ( Model student (List.filterMap zipWithStory answers) Nothing Nothing False False, newSession )
    in
        Task.map3 mkModel (Session.loadStories session) loadStudent loadAnswers
            |> Task.mapError handleLoadError


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        ShowChangePassword ->
            { model | changePasswordForm = Just <| ChangePassword.init (.id model.student) 8 }
                => Cmd.none

        DismissChangePassword ->
            { model | changePasswordForm = Nothing }
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

        DismissChangeUsername ->
            { model | changeUsernameForm = Nothing }
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
                model
                    => (Api.postSchoolStudentsByStudentId (authorization session) s.id newStudent
                            |> Http.send UpdateStudentResponse
                       )

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

        DismissConfirmDelete ->
            { model | showConfirmDelete = False } => Cmd.none

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


view : Model -> Html Msg
view model =
    div [ class "container page" ]
        [ h3 [] [ text (.name model.student) ]
        , viewToolbar model.student
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
    row
        [ toolbar "toolbar"
            [ btnGroup
                [ button [ class "btn btn-default", onClick ShowChangePassword ] [ text "Change password" ]
                , button [ class "btn btn-default", onClick ShowChangeUsername ] [ text "Change username" ]
                , button [ class "btn btn-default", onClick ToggleDeletedStatus ]
                    [ text
                        (case student.deleted of
                            Nothing ->
                                "Delete"

                            _ ->
                                "Un-delete"
                        )
                    ]
                , button [ class "btn btn-default", onClick ToggleHiddenStatus ]
                    [ text
                        (if student.hidden then
                            "Un-hide"
                         else
                            "Hide"
                        )
                    ]
                ]
            ]
        ]


changePasswordDialog : ChangePassword.Model -> Dialog.Config Msg
changePasswordDialog form =
    { closeMessage = Just DismissChangePassword
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Change password" ])
    , body =
        Just <|
            div []
                [ ChangePassword.view form
                    |> Html.map ChangePasswordMsg
                ]
    , footer = Nothing
    }


changeUsernameDialog : ChangeUsername.Model -> Dialog.Config Msg
changeUsernameDialog form =
    { closeMessage = Just DismissChangeUsername
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Change username" ])
    , body =
        Just <|
            div []
                [ ChangeUsername.view form
                    |> Html.map ChangeUsernameMsg
                ]
    , footer = Nothing
    }


confirmDeleteDialog : Dialog.Config Msg
confirmDeleteDialog =
    { closeMessage = Just DismissConfirmDelete
    , containerClass = Nothing
    , header = Nothing
    , body =
        Just <|
            div []
                [ p [] [ text "Are you sure you want to delete this student account? It will be marked for deletion and removed automatically at a later date (you can un-delete it if you change your mind)." ]
                , button [ class "btn btn-default", onClick ConfirmDelete ]
                    [ text "Delete student"
                    ]
                ]
    , footer = Nothing
    }
