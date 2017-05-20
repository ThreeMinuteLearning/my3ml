module Page.Home exposing (view, init)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Views.Page as Page
import Views.RobotPanel as RobotPanel
import Views.StoryTiles as StoryTiles


init : Session -> Task PageLoadError Session
init session =
    let
        handleLoadError _ =
            pageLoadError Page.Home "Couldn't load stories for the home page."
    in
        Session.loadStories session
            |> Task.mapError handleLoadError


view : Session -> Html msg
view session =
    div [ class "home-page" ]
        [ div [ class "container page" ]
            [ RobotPanel.view
            , div []
                [ h2 [] [ text "Starter Stories" ]
                , StoryTiles.view (List.take 24 session.stories)
                ]
            ]
        ]
