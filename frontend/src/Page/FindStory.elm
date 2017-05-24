module Page.FindStory exposing (Model, Msg, init, view, update)

import Api
import Bootstrap
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Regex
import Route
import Table
import Task exposing (Task)
import Views.Page as Page
import Views.RobotPanel as RobotPanel


type alias Model =
    { storyFilter : String
    , tableState : Table.State
    }


type Msg
    = StoryFilterInput String
    | SetTableState Table.State


initialModel : Model
initialModel =
    Model "" (Table.initialSort "Title")


init : Session -> Task PageLoadError ( Model, Session )
init session =
    let
        handleLoadError _ =
            pageLoadError Page.Other "There was a problem loading the stories."
    in
        Session.loadStories session
            |> Task.map ((,) initialModel)
            |> Task.mapError handleLoadError


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        StoryFilterInput f ->
            { model | storyFilter = f } ! []

        SetTableState t ->
            { model | tableState = t } ! []


view : Session -> Model -> Html Msg
view { cache } m =
    div [ class "container page" ]
        [ RobotPanel.view
        , div []
            [ div [ class "form-group" ]
                [ input
                    [ type_ "text"
                    , value m.storyFilter
                    , onInput StoryFilterInput
                    , placeholder "Search text"
                    , id "storyfilter"
                    ]
                    []
                ]
            ]
        , div [ class "table-responsive" ]
            [ Table.view tableConfig m.tableState (filterStories m.storyFilter cache.stories) ]
        ]


filterStories : String -> List Api.Story -> List Api.Story
filterStories storyFilter stories =
    if String.length storyFilter < 3 then
        stories
    else
        let
            r =
                Regex.caseInsensitive (Regex.regex storyFilter)

            match story =
                Regex.contains r story.title || Regex.contains r story.content
        in
            List.filter match stories


tableConfig : Table.Config Api.Story Msg
tableConfig =
    let
        tag i s =
            s.tags
                |> List.drop (i - 1)
                |> List.head
                |> Maybe.withDefault ""

        -- This is needed to make the level column wide enough so the heading and arrow
        -- don't wrap
        levelColumn =
            Table.veryCustomColumn
                { name = "Level"
                , viewData = \s -> Table.HtmlDetails [ style [ ( "width", "6em" ) ] ] [ Html.text (toString s.level) ]
                , sorter = Table.increasingOrDecreasingBy .level
                }

        storyTitleColumn =
            Table.veryCustomColumn
                { name = "Title"
                , viewData = viewStoryLink
                , sorter = Table.increasingOrDecreasingBy .title
                }

        viewStoryLink s =
            Table.HtmlDetails []
                [ Html.a [ Html.Attributes.href (Route.routeToString (Route.Story s.id)) ] [ text s.title ]
                ]
    in
        Table.customConfig
            { toId = .id
            , toMsg = SetTableState
            , columns =
                [ storyTitleColumn
                , Table.stringColumn "Curriculum" .curriculum
                , Table.stringColumn "Tag2" (tag 1)
                , Table.stringColumn "Tag3" (tag 2)
                , levelColumn
                ]
            , customizations = Bootstrap.tableCustomizations
            }
