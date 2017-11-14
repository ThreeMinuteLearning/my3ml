module Page.FindStory exposing (Model, Msg, init, view, subscriptions, update)

import Api
import Bootstrap exposing (closeBtn)
import Data.Session as Session exposing (Session, Cache, authorization, findStoryById, isEditor)
import Data.Settings exposing (Settings, defaultSettings)
import Exts.List exposing (firstMatch)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)
import Http
import List.InfiniteZipper as Zipper exposing (InfiniteZipper)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Ports
import Regex
import Route
import Table
import Task exposing (Task)
import Views.Form as Form
import Util exposing ((=>), onClickPreventDefault, viewIf, viewUnless, defaultHttpErrorMsg)
import Views.Story as StoryView
import Views.StoryTiles as StoryTiles
import Window


type alias Model =
    { errors : List Error
    , storyFilter : String
    , showDisabledStoriesOnly : Bool
    , stories : List Api.Story
    , tableState : Table.State
    , browser : Maybe (InfiniteZipper Api.Story)
    , storyView : StoryView.State
    , viewType : ViewType
    , windowSize : Window.Size
    , selectedStories : List Api.Story
    , anthologyForm : Maybe AnthologyForm
    }


type alias AnthologyForm =
    String


type Msg
    = StoryFilterInput String
    | StoryViewMsg StoryView.Msg
    | SetTableState Table.State
    | BrowseFrom Int
    | Next
    | Previous
    | Scroll Bool
    | Resize Window.Size
    | CloseBrowser
    | ToggleShowDisabledOnly
    | ClearSelectedStories
    | SelectStory Api.Story
    | CreateAnthology
    | SetAnthologyName String
    | SetViewType ViewType
    | SubmitAnthologyForm
    | CreateAnthologyResponse (Result Http.Error Api.Anthology)
    | DeleteAnthology String
    | DeleteAnthologyResponse (Result Http.Error String)
    | SetStarterStories String
    | SetStarterStoriesResponse (Result Http.Error Api.NoContent)
    | HideAnthology Api.Anthology
    | HideAnthologyResponse (Result Http.Error Api.Anthology)


type ViewType
    = Tiles Int
    | Table
    | Anthologies


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
                Tiles (StoryTiles.tilesPerPage size)
            else
                Table
    in
        Model [] "" False stories (Table.initialSort sortColumn) Nothing (StoryView.init size) viewType size [] Nothing
            => session


init : Session -> Task PageLoadError ( Model, Session )
init session =
    let
        handleLoadError e =
            pageLoadError e ("There was a problem loading the stories: " ++ defaultHttpErrorMsg e ++ ".")

        loadData =
            Session.loadStories session
                |> Task.andThen Session.loadAnthologies
                |> Task.mapError handleLoadError
    in
        Task.map2 initialModel loadData Window.size


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.browser of
        Nothing ->
            case m.viewType of
                Tiles _ ->
                    Sub.batch [ Window.resizes Resize, Ports.scroll Scroll, Ports.lastEltVisible Scroll ]

                _ ->
                    Sub.none

        Just _ ->
            Sub.map StoryViewMsg StoryView.subscriptions


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Session )
update session msg model =
    case msg of
        StoryFilterInput f ->
            { model | storyFilter = f, stories = filterStories f session.cache.stories }
                => Cmd.none
                => session

        SetTableState t ->
            { model | tableState = t }
                => Cmd.none
                => session

        BrowseFrom storyId ->
            { model | browser = zipperFrom storyId model.stories }
                => Cmd.none
                => session

        StoryViewMsg svm ->
            let
                ( newStoryView, cmd ) =
                    StoryView.update svm model.storyView
            in
                { model | storyView = newStoryView }
                    => Cmd.map StoryViewMsg cmd
                    => session

        Next ->
            { model | browser = model.browser |> Maybe.map Zipper.next }
                => Cmd.none
                => session

        Previous ->
            { model | browser = model.browser |> Maybe.map Zipper.previous }
                => Cmd.none
                => session

        Scroll atBottom ->
            if atBottom then
                ( loadMore model, session )
            else
                ( ( model, Cmd.none ), session )

        Resize s ->
            { model | windowSize = s } => Cmd.none => session

        CloseBrowser ->
            { model | browser = Nothing } => Cmd.none => session

        ToggleShowDisabledOnly ->
            let
                flag =
                    not model.showDisabledStoriesOnly

                newStories =
                    if flag then
                        List.filter (\s -> not s.enabled) model.stories
                    else
                        filterStories model.storyFilter session.cache.stories
            in
                { model | showDisabledStoriesOnly = flag, stories = newStories }
                    => Cmd.none
                    => session

        SelectStory s ->
            { model | selectedStories = s :: model.selectedStories }
                => Cmd.none
                => session

        ClearSelectedStories ->
            { model | selectedStories = [] } => Cmd.none => session

        CreateAnthology ->
            { model | anthologyForm = Just "" } => Cmd.none => session

        SetAnthologyName n ->
            case model.anthologyForm of
                Just _ ->
                    { model | anthologyForm = Just n } => Cmd.none => session

                Nothing ->
                    ( ( model, Cmd.none ), session )

        SubmitAnthologyForm ->
            case model.anthologyForm of
                Just f ->
                    case validateAnthologyForm f of
                        [] ->
                            { model | errors = [], anthologyForm = Nothing }
                                => (Api.postAnthologies (authorization session) (Api.Anthology "" f (Just "") (List.map .id model.selectedStories) False)
                                        |> Http.send CreateAnthologyResponse
                                   )
                                => session

                        errors ->
                            { model | errors = errors }
                                => Cmd.none
                                => session

                Nothing ->
                    ( ( model, Cmd.none ), session )

        CreateAnthologyResponse (Ok newAnthology) ->
            { model | anthologyForm = Nothing }
                => Cmd.none
                => updateAnthologies (\anthologies -> newAnthology :: anthologies) session

        CreateAnthologyResponse (Err e) ->
            { model | errors = [ "Couldn't create the anthology: " ++ defaultHttpErrorMsg e ] }
                => Cmd.none
                => session

        SetViewType v ->
            { model | viewType = v } => Cmd.none => session

        DeleteAnthology aid ->
            model
                => (Api.deleteAnthologiesByAnthologyId (authorization session) aid
                        |> Http.send DeleteAnthologyResponse
                   )
                => session

        DeleteAnthologyResponse (Ok aid) ->
            ( model, Cmd.none )
                => updateAnthologies (List.filter (\a -> a.id /= aid)) session

        DeleteAnthologyResponse (Err e) ->
            { model | errors = [ "Couldn't delete the anthology: " ++ defaultHttpErrorMsg e ] }
                => Cmd.none
                => session

        SetStarterStories aid ->
            model
                => (Api.postAnthologiesByAnthologyIdStarter_stories (authorization session) aid
                        |> Http.send SetStarterStoriesResponse
                   )
                => session

        SetStarterStoriesResponse (Ok _) ->
            ( model, Cmd.none ) => session

        SetStarterStoriesResponse (Err e) ->
            { model | errors = [ "Couldn't set the starter stories: " ++ defaultHttpErrorMsg e ] }
                => Cmd.none
                => session

        HideAnthology a ->
            model
                => (Api.postAnthologiesByAnthologyId (authorization session) a.id ({ a | hidden = not a.hidden })
                        |> Http.send HideAnthologyResponse
                   )
                => session

        HideAnthologyResponse (Ok newA) ->
            ( model, Cmd.none )
                => updateAnthologies
                    (List.map
                        (\a ->
                            if a.id == newA.id then
                                newA
                            else
                                a
                        )
                    )
                    session

        HideAnthologyResponse (Err e) ->
            { model | errors = [ "Couldn't update the anthology: " ++ defaultHttpErrorMsg e ] }
                => Cmd.none
                => session


updateAnthologies : (List Api.Anthology -> List Api.Anthology) -> Session -> Session
updateAnthologies f =
    Session.updateCache (\c -> { c | anthologies = f c.anthologies })


type alias Error =
    String


validateAnthologyForm : AnthologyForm -> List Error
validateAnthologyForm f =
    if String.length f <= 3 then
        [ "The anthology name must be more than 3 characters long" ]
    else
        []


loadMore : Model -> ( Model, Cmd msg )
loadMore m =
    case m.viewType of
        Tiles n ->
            if n >= List.length m.stories then
                ( m, Cmd.none )
            else
                { m | viewType = Tiles (n + (2 * (StoryTiles.tilesPerRow m.windowSize))) }
                    => Ports.isLastEltVisible ("storytiles")

        _ ->
            ( m, Cmd.none )


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
                    [ viewStoriesFilter session m
                    , Form.viewErrorMsgs m.errors
                    , viewUnless (List.isEmpty m.selectedStories) (viewSelectedStories m)
                    , case m.viewType of
                        Tiles n ->
                            StoryTiles.view (List.take n m.stories)

                        Table ->
                            viewStoriesTable m

                        Anthologies ->
                            viewAnthologies session
                    ]

            Just b ->
                div []
                    [ viewBrowserToolbar session (Zipper.current b) (m.selectedStories)
                    , Html.map StoryViewMsg <| StoryView.view (settingsFromSession session) (Zipper.current b) m.storyView
                    ]
        ]


settingsFromSession : Session -> Settings
settingsFromSession session =
    session.user
        |> Maybe.map .settings
        |> Maybe.withDefault defaultSettings


viewBrowserToolbar : Session -> Api.Story -> List Api.Story -> Html Msg
viewBrowserToolbar session s selected =
    nav []
        [ ul [ class "pager" ]
            [ li [ class "previous" ] [ a [ href "#", onClickPreventDefault Previous ] [ text "Prev" ] ]
            , viewIf (isEditor session) <| li [] [ a [ href (Route.routeToString (Route.Editor s.id)) ] [ text "Edit" ] ]
            , li [] [ a [ href "#", onClickPreventDefault CloseBrowser ] [ text "Back to stories" ] ]
            , viewUnless (Session.isStudent session || List.member s selected) <| li [] [ a [ href "#", onClickPreventDefault (SelectStory s) ] [ text "Select story" ] ]
            , viewIf (Session.isStudent session) <| li [] [ a [ href (Route.routeToString (Route.Story s.id)) ] [ text "Work on story" ] ]
            , li [ class "next" ] [ a [ class "pull-right", href "#", onClickPreventDefault Next ] [ text "Next" ] ]
            ]
        ]


viewStoriesFilter : Session -> Model -> Html Msg
viewStoriesFilter session m =
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
            [ text (" " ++ toString (List.length m.stories) ++ " matching stories ")
            ]
        , viewIf (isEditor session) (viewToggleDisabledStoriesOnly m)
        , text "  "
        , viewCycleDisplayButton m
        ]


viewCycleDisplayButton : Model -> Html Msg
viewCycleDisplayButton m =
    let
        ( nextViewType, displayTxt ) =
            case m.viewType of
                Table ->
                    ( Anthologies, "View anthologies" )

                Tiles _ ->
                    ( Table, "View table" )

                Anthologies ->
                    ( Tiles (StoryTiles.tilesPerPage m.windowSize), "View tiles" )
    in
        button [ class "btn btn-default", onClick (SetViewType nextViewType) ] [ text displayTxt ]


viewToggleDisabledStoriesOnly : Model -> Html Msg
viewToggleDisabledStoriesOnly m =
    let
        txt =
            if m.showDisabledStoriesOnly then
                "Show all stories"
            else
                "Hide enabled stories"
    in
        a [ href "#", onClickPreventDefault ToggleShowDisabledOnly ] [ text txt ]


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
                , viewData = titleColumnData
                , sorter = Table.increasingOrDecreasingBy .title
                }

        titleColumnData s =
            Table.HtmlDetails []
                [ viewStoryLink s
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


viewStoryLink : Api.Story -> Html Msg
viewStoryLink s =
    Html.a [ href "#", onClickPreventDefault (BrowseFrom s.id) ] [ text s.title ]


viewSelectedStories : Model -> Html Msg
viewSelectedStories m =
    let
        createAnthology =
            case m.anthologyForm of
                Nothing ->
                    button [ class "btn btn-default", onClick CreateAnthology ] [ text "Create Anthology" ]

                Just f ->
                    Html.form [ onSubmit SubmitAnthologyForm ]
                        [ Form.input
                            [ class "form-control-lg"
                            , placeholder "Anthology name"
                            , tabindex 1
                            , onInput SetAnthologyName
                            ]
                            []
                        , submitButton
                        ]

        submitButton =
            Html.button [ class "btn btn-primary pull-xs-right", tabindex 2 ] [ text "Save anthology" ]
    in
        div [ class "panel panel-default" ]
            [ div [ class "panel-heading" ]
                [ div [ class "btn-group pull-right" ]
                    [ closeBtn ClearSelectedStories ]
                , h4 [ class "panel-title" ] [ text "Selected Stories" ]
                ]
            , div [ class "panel-body" ]
                [ viewStoryTable m.selectedStories
                , createAnthology
                ]
            ]


viewStoryTable : List Api.Story -> Html Msg
viewStoryTable stories =
    let
        storyRow s =
            tr []
                [ td [] [ viewStoryLink s ]
                ]
    in
        table [ class "table" ]
            [ tbody [] (List.map storyRow stories) ]


viewAnthologies : Session -> Html Msg
viewAnthologies session =
    let
        go =
            List.map pickStories session.cache.anthologies

        pickStories a =
            ( a, List.filterMap (findStoryById session.cache) a.stories )

        canDelete a =
            (a.schoolId /= Nothing && Session.isTeacher session) || Session.isEditor session

        render ( a, astories ) =
            div [ class "anthology" ]
                [ h4 [] [ text a.name ]
                , viewIf (canDelete a)
                    (button
                        [ class "btn btn-default btn-xs"
                        , onClick (DeleteAnthology a.id)
                        ]
                        [ text "Delete" ]
                    )
                , viewIf (isEditor session)
                    (button
                        [ class "btn btn-default btn-xs"
                        , onClick (HideAnthology a)
                        ]
                        [ text
                            (if a.hidden then
                                "Un-hide"
                             else
                                "Hide"
                            )
                        ]
                    )
                , viewIf (isEditor session)
                    (button
                        [ class "btn btn-default btn-xs"
                        , onClick (SetStarterStories a.id)
                        , disabled (List.length a.stories < 24)
                        ]
                        [ text "Set Starter Stories" ]
                    )
                , viewStoryTable astories
                ]
    in
        div [ class "anthologies" ]
            (List.map render go)
