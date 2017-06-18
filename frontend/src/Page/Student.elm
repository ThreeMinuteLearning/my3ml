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
    }


type Msg
    = ShowChangePassword
    | DismissChangePassword
    | ChangePasswordMsg ChangePassword.Msg
    | ShowChangeUsername
    | DismissChangeUsername
    | ChangeUsernameMsg ChangeUsername.Msg


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
            ( Model student (List.filterMap zipWithStory answers) Nothing Nothing, newSession )
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


view : Model -> Html Msg
view model =
    div [ class "container page" ]
        [ h3 [] [ text (.name model.student) ]
        , viewToolbar
        , Dialog.view (Maybe.map changePasswordDialog model.changePasswordForm)
        , Dialog.view (Maybe.map changeUsernameDialog model.changeUsernameForm)
        ]


viewToolbar : Html Msg
viewToolbar =
    row
        [ toolbar "toolbar"
            [ btnGroup
                [ button [ class "btn btn-default", onClick ShowChangePassword ] [ text "Change password" ]
                , button [ class "btn btn-default", onClick ShowChangeUsername ] [ text "Change username" ]
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
