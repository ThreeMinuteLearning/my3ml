module Page.Student exposing (Model, Msg, init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Util exposing ((=>))
import Views.Page as Page


type alias Model =
    { student : Api.Student
    , answers : List ( Api.Answer, Api.Story )
    }


type Msg
    = NoOp


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
            ( Model student (List.filterMap zipWithStory answers), newSession )
    in
        Task.map3 mkModel (Session.loadStories session) loadStudent loadAnswers
            |> Task.mapError handleLoadError


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    model => Cmd.none


view : Model -> Html Msg
view model =
    div [ class "container page" ]
        [ text (.name model.student) ]
