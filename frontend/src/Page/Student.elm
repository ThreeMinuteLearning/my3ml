module Page.Student exposing (Model, Msg, init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Dialog
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Util exposing ((=>))
import Views.ChangePasswordForm as ChangePassword
import Views.Page as Page


type alias Model =
    { student : Api.Student
    , answers : List ( Api.Answer, Api.Story )
    , changePasswordForm : Maybe ChangePassword.Model
    }


type Msg
    = ShowChangePassword
    | DismissChangePassword
    | ChangePasswordMsg ChangePassword.Msg


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
            ( Model student (List.filterMap zipWithStory answers) Nothing, newSession )
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


view : Model -> Html Msg
view model =
    div [ class "container page" ]
        [ text (.name model.student)
        , button [ onClick ShowChangePassword ] [ text "Change Password" ]
        , Dialog.view (Maybe.map changePasswordDialog model.changePasswordForm)
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
