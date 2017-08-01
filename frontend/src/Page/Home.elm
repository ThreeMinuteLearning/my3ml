module Page.Home exposing (Model, view, update, init)

import Api
import Data.Session as Session exposing (Session, User, Role(..))
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Util exposing ((=>))
import Views.RobotPanel as RobotPanel
import Views.StoryTiles as StoryTiles


type alias Model =
    { stories : List Api.Story
    }


init : Session -> Task PageLoadError ( Model, Session )
init session =
    let
        handleLoadError e =
            pageLoadError e "Couldn't load stories for the home page."
    in
        Session.loadStories session
            |> Task.andThen Session.loadUserAnswers
            |> Task.map initModel
            |> Task.mapError handleLoadError


initModel : Session -> ( Model, Session )
initModel session =
    (flip (,) session) <|
        case Maybe.map (\u -> ( u.role, u.level )) session.user of
            Just ( Student, level ) ->
                Model (pickStories level session.cache.answers session.cache.stories)

            _ ->
                Model session.cache.stories


pickStories : Int -> Dict String Api.Answer -> List Api.Story -> List Api.Story
pickStories level answers stories =
    let
        storiesForLevel =
            List.filter (\s -> s.level < level + 2) stories

        unansweredStories =
            List.filter (\s -> not (Dict.member s.id answers)) storiesForLevel

        -- Take n items of level l from list ss
        takeLevel l n acc ss =
            case ( n, ss ) of
                ( 0, _ ) ->
                    acc

                ( _, [] ) ->
                    acc

                ( _, s :: rem ) ->
                    if s.level == l then
                        takeLevel l (n - 1) (s :: acc) rem
                    else
                        takeLevel l n acc rem

        storiesPerLevel =
            List.indexedMap (,) <|
                case level of
                    0 ->
                        [ 25, 5 ]

                    1 ->
                        [ 10, 15, 5 ]

                    2 ->
                        [ 5, 10, 10, 5 ]

                    3 ->
                        [ 3, 3, 5, 10, 5 ]

                    4 ->
                        [ 2, 3, 3, 3, 10, 5 ]

                    5 ->
                        [ 2, 2, 2, 2, 5, 10, 5 ]

                    6 ->
                        [ 2, 2, 2, 2, 5, 5, 10, 5 ]

                    7 ->
                        [ 0, 2, 2, 2, 5, 5, 5, 10, 5 ]

                    _ ->
                        [ 0, 0, 0, 2, 2, 5, 5, 5, 10, 5 ]
    in
        if Dict.size answers < 20 then
            List.concatMap (\( l, n ) -> takeLevel l n [] unansweredStories) storiesPerLevel
        else
            sortForLevel level unansweredStories


sortForLevel : Int -> List Api.Story -> List Api.Story
sortForLevel l stories =
    List.sortBy (\s -> abs (s.level - l)) stories


update : Session -> Model -> ( Model, Cmd msg )
update session model =
    model => Cmd.none


view : Session -> Model -> Html msg
view _ model =
    div [ class "home-page" ]
        [ div [ class "container page" ]
            [ RobotPanel.view
            , div []
                [ h2 [] [ text "Starter Stories" ]
                , StoryTiles.view (List.take 24 model.stories)
                ]
            ]
        ]
