module Page.Home exposing (Model, init, update, view)

import Api
import Data.Session as Session exposing (Role(..), Session, User)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import List.Extra as List
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Tuple exposing (first, pair)
import Util exposing (defaultHttpErrorMsg)
import Views.RobotPanel as RobotPanel
import Views.StoryTiles as StoryTiles


type alias Model =
    { stories : List Api.Story
    }


init : Session -> Task PageLoadError ( Model, Session )
init session =
    let
        handleLoadError e =
            pageLoadError e ("Couldn't load stories for the home page: " ++ defaultHttpErrorMsg e ++ ".")
    in
        Session.loadStories session
            |> Task.andThen Session.loadUserAnswers
            |> Task.map initModel
            |> Task.mapError handleLoadError


initModel : Session -> ( Model, Session )
initModel session =
    let
        model =
            case Maybe.map (\u -> ( u.role, u.level )) session.user of
                Just ( Student, level ) ->
                    Model (pickStories session level)

                _ ->
                    Model session.cache.stories
    in
        (model, session)


isBeginner : Session -> Bool
isBeginner =
    (>) 20 << Dict.size << .answers << .cache


pickStories : Session -> Int -> List Api.Story
pickStories session level =
    let
        answers =
            session.cache.answers

        stories =
            List.filter (\s -> s.level < level + 2) session.cache.stories

        unansweredStories =
            List.filter (\s -> not (Dict.member s.id answers)) stories

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
            List.indexedMap pair <|
                case level of
                    0 ->
                        [ 25, 5, 0, 0, 0, 0 ]

                    1 ->
                        [ 10, 15, 5, 0, 0, 0 ]

                    2 ->
                        [ 5, 10, 10, 5, 0, 0 ]

                    3 ->
                        [ 3, 3, 5, 10, 5, 0, 0 ]

                    4 ->
                        [ 2, 3, 3, 3, 10, 5, 0, 0 ]

                    5 ->
                        [ 2, 2, 2, 2, 5, 10, 5, 0 ]

                    6 ->
                        [ 2, 2, 2, 2, 5, 5, 10, 5, 0 ]

                    7 ->
                        [ 0, 2, 2, 2, 5, 5, 5, 10, 5, 0 ]

                    _ ->
                        [ 0, 0, 0, 2, 2, 5, 5, 5, 10, 5, 0 ]
    in
        if isBeginner session then
            List.concatMap (\( l, n ) -> takeLevel l n [] unansweredStories) (bumpLevels (answerLevels session answers) storiesPerLevel)

        else
            sortForLevel level unansweredStories


answerLevels : Session -> Dict Int Api.Answer -> Dict Int Int
answerLevels session answers =
    let
        answerLevel a =
            Session.findStoryById session.cache a.storyId
                |> Maybe.map .level
                |> Maybe.withDefault 0

        answersCompletedForLevel ( _, l ) =
            1 + List.length l
    in
        List.map answerLevel (Dict.values answers)
            |> List.sort
            |> List.group
        |> (\l ->
                List.map2 pair (List.map first l) (List.map answersCompletedForLevel l)
                    |> Dict.fromList
           )


bumpLevels : Dict Int Int -> List ( Int, Int ) -> List ( Int, Int )
bumpLevels answers nPerLevel =
    let
        nAnswers l =
            Maybe.withDefault 0 (Dict.get l answers)

        bumpLevel ( l, n ) =
            ( l, max 0 (n - nAnswers l + nAnswers (l - 1)) )
    in
        List.map bumpLevel nPerLevel


sortForLevel : Int -> List Api.Story -> List Api.Story
sortForLevel l stories =
    List.sortBy (\s -> abs (s.level - l)) stories


update : Session -> Model -> ( Model, Cmd msg )
update session model =
    ( model, Cmd.none )


view : Session -> Model -> { title: String, content: Html msg }
view session model =
    { title = "Home"
    , content =
        div [ class "home-page" ]
        [ div [ class "container page" ]
            [ RobotPanel.view
            , div []
                [ h1 []
                    [ text (storiesTitle session)
                    ]
                , StoryTiles.view False (List.take 24 model.stories)
                ]
            ]
        ]
    }


storiesTitle : Session -> String
storiesTitle session =
    if Session.isStudent session then
        if isBeginner session then
            "Starter Stories"

        else
            "My Stories"

    else
        "Sample Stories"
