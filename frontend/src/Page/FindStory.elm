module Page.FindStory exposing (Model, Msg, init, view, subscriptions, update)

import Api
import Bootstrap
import Data.Session as Session exposing (Session)
import Data.Settings exposing (Settings, defaultSettings)
import Exts.List exposing (firstMatch)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import List.InfiniteZipper as Zipper exposing (InfiniteZipper)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Regex
import Table
import Task exposing (Task)
import Util exposing ((=>), onClickPreventDefault)
import Views.Story as StoryView
import Views.StoryTiles as StoryTiles
import Window


type alias Model =
    { storyFilter : String
    , stories : List Api.Story
    , tableState : Table.State
    , browser : Maybe (InfiniteZipper Api.Story)
    , storyView : StoryView.State
    , viewType : ViewType
    , selectedStories : List Api.Story
    }


type Msg
    = StoryFilterInput String
    | StoryViewMsg StoryView.Msg
    | SetTableState Table.State
    | BrowseFrom Int
    | Next
    | Previous


type ViewType
    = Tiles
    | Table


initialModel : Session -> Window.Size -> ( Model, Session )
initialModel session size =
    let
        sortColumn =
            if Session.isStudent session then
                ""
            else
                "Title"

        stories =
            session.cache.stories

        viewType =
            if Session.isStudent session then
                Tiles
            else
                Table
    in
        Model "" stories (Table.initialSort sortColumn) Nothing (StoryView.init size) viewType [] => session


init : Session -> Task PageLoadError ( Model, Session )
init session =
    let
        handleLoadError e =
            pageLoadError e "There was a problem loading the stories."
    in
        Task.map2 initialModel (Session.loadStories session) Window.size
            |> Task.mapError handleLoadError


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.browser of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.map StoryViewMsg StoryView.subscriptions


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update { cache } msg model =
    case msg of
        StoryFilterInput f ->
            { model | storyFilter = f, stories = filterStories model.storyFilter cache.stories } ! []

        SetTableState t ->
            { model | tableState = t } ! []

        BrowseFrom storyId ->
            { model | browser = zipperFrom storyId model.stories } ! []

        StoryViewMsg svm ->
            let
                ( newStoryView, cmd ) =
                    StoryView.update svm model.storyView
            in
                { model | storyView = newStoryView } => Cmd.map StoryViewMsg cmd

        Next ->
            { model | browser = model.browser |> Maybe.map Zipper.next } => Cmd.none

        Previous ->
            { model | browser = model.browser |> Maybe.map Zipper.previous } => Cmd.none


zipperFrom : Int -> List Api.Story -> Maybe (InfiniteZipper Api.Story)
zipperFrom storyId stories =
    Zipper.fromList stories
        |> Maybe.andThen (Zipper.findFirst (\s -> s.id == storyId))


view : Session -> Model -> Html Msg
view session m =
    div [ class "container page" ]
        [ case m.browser of
            Nothing ->
                div []
                    [ viewStoriesFilter m
                    , case m.viewType of
                        Tiles ->
                            StoryTiles.view m.stories

                        Table ->
                            viewStoriesTable m
                    ]

            Just b ->
                viewBrowser (settingsFromSession session) (Zipper.current b) m.storyView
        ]


settingsFromSession : Session -> Settings
settingsFromSession session =
    session.user
        |> Maybe.map .settings
        |> Maybe.withDefault defaultSettings


viewBrowser : Settings -> Api.Story -> StoryView.State -> Html Msg
viewBrowser settings story storyView =
    div []
        [ viewBrowserToolbar
        , Html.map StoryViewMsg <| StoryView.view settings story storyView
        ]


viewBrowserToolbar : Html Msg
viewBrowserToolbar =
    div []
        [ a [ href "#", onClickPreventDefault Previous ] [ text "prev" ]
        , a [ class "pull-right", href "#", onClickPreventDefault Next ] [ text "next" ]
        ]


viewStoriesFilter : Model -> Html Msg
viewStoriesFilter m =
    div [ class "form-group" ]
        [ input
            [ type_ "text"
            , value m.storyFilter
            , onInput StoryFilterInput
            , placeholder "Search text"
            , id "storyfilter"
            ]
            []
        , label [ style [ ( "margin-left", "5px" ) ], for "storyfilter" ]
            [ text (" " ++ toString (List.length m.stories) ++ " matching stories")
            ]
        ]


viewStoriesTable : Model -> Html Msg
viewStoriesTable m =
    div [ class "table-responsive" ]
        [ Table.view tableConfig m.tableState m.stories ]


filterStories : String -> List Api.Story -> List Api.Story
filterStories storyFilter stories =
    if String.length storyFilter < 3 then
        stories
    else
        let
            r =
                Regex.caseInsensitive (Regex.regex storyFilter)

            tagMatch tags =
                (firstMatch (Regex.contains r) tags) /= Nothing

            match story =
                Regex.contains r story.title || Regex.contains r story.qualification || tagMatch story.tags || Regex.contains r story.content
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
                [ Html.a [ href "#", onClickPreventDefault (BrowseFrom s.id) ] [ text s.title ]
                ]
    in
        Table.customConfig
            { toId = toString << .id
            , toMsg = SetTableState
            , columns =
                [ storyTitleColumn
                , Table.stringColumn "General" (tag 1)
                , Table.stringColumn "BGE" (Maybe.withDefault "" << .curriculum)
                , Table.stringColumn "SQA" .qualification
                , levelColumn
                ]
            , customizations = Bootstrap.tableCustomizations
            }
