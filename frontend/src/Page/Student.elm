module Page.Student exposing (Model, Msg, init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Util exposing ((=>))
import Views.Page as Page


type alias Model =
    { student : Api.Student
    }


type Msg
    = NoOp


init : Session -> String -> Task PageLoadError ( Model, Session )
init session slug =
    let
        handleLoadError _ =
            pageLoadError Page.Other "Unable to load data for page."
    in
        Api.getSchoolStudentsByStudentId (authorization session) slug
            |> Http.toTask
            |> Task.mapError handleLoadError
            |> Task.map (\s -> ( Model s, session ))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    model => Cmd.none


view : Model -> Html Msg
view model =
    div [ class "container page" ]
        [ text (.name model.student) ]
