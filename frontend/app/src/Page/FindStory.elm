module Page.FindStory exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Bootstrap exposing (closeBtn)
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Cache exposing (Cache)
import Components
import Data.Session as Session exposing (Session, authorization, findStoryById, isEditor, isStudent, updateCache)
import Data.Settings exposing (Settings, defaultSettings)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import List.Extra
import List.Zipper.Infinite as Zipper exposing (Zipper)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Ports
import Regex
import Route
import Table
import Task exposing (Task)
import Tuple exposing (first)
import Util exposing (defaultHttpErrorMsg, viewIf, viewUnless)
import Views.Form as Form
import Views.Story as StoryView
import Views.StoryTiles as StoryTiles


type alias Model =
    { errors : List Error
    , storyFilter : String
    , showDisabledStoriesOnly : Bool
    , stories : List Api.Story
    , tableState : Table.State
    , browser : Maybe (Zipper Api.Story)
    , storyView : StoryView.State
    , viewType : ViewType
    , windowSize : ( Int, Int )
    , anthologyForm : Maybe AnthologyForm
    }


type alias AnthologyForm =
    { name : String
    , description : String
    }


type Msg
    = StoryFilterInput String
    | StoryViewMsg StoryView.Msg
    | SetTableState Table.State
    | BrowseFrom Int
    | Next
    | Previous
    | Scroll Bool
    | Resize Int Int
    | CloseBrowser
    | ToggleShowDisabledOnly
    | ClearSelectedStories
    | SelectStory Api.Story
    | CreateAnthology
    | SetAnthologyName String
    | SetAnthologyDescription String
    | SetViewType ViewType
    | SubmitAnthologyForm
    | CreateAnthologyResponse (Result Http.Error Api.Anthology)
    | DeleteAnthology String
    | DeleteAnthologyResponse (Result Http.Error String)
    | SetStarterStories String
    | SetStarterStoriesResponse (Result Http.Error Api.NoContent)
    | UpdateAnthology Api.Anthology
    | UpdateAnthologyResponse (Result Http.Error Api.Anthology)
    | AddAnthologyToWorkQueue (List Api.Story)
    | SaveWorkQueueResponse (Result Http.Error Api.NoContent)


type ViewType
    = Tiles Int
    | Table
    | Anthologies


initialModel : Session -> Browser.Dom.Viewport -> ( Model, Session )
initialModel session { viewport } =
    let
        sortColumn =
            if Session.isStudent session then
                ""

            else
                "Title"

        maxWidth =
            990

        size =
            ( Basics.min maxWidth (round viewport.width), round viewport.height )

        viewType =
            if Session.isStudent session then
                Tiles (StoryTiles.tilesPerPage size)

            else
                Table
    in
    ( { errors = []
      , storyFilter = ""
      , showDisabledStoriesOnly = False
      , stories = (Session.getCache session).stories
      , tableState = Table.initialSort sortColumn
      , browser = Nothing
      , storyView = StoryView.init (first size)
      , viewType = viewType
      , windowSize = size
      , anthologyForm = Nothing
      }
    , session
    )


init : Session -> Task PageLoadError ( Model, Session )
init session =
    let
        handleLoadError e =
            pageLoadError e ("There was a problem loading the stories: " ++ defaultHttpErrorMsg e ++ ".")

        loadData =
            Session.loadStories session
                |> Task.andThen Session.loadUserAnswers
                |> Task.andThen Session.loadAnthologies
                |> Task.mapError handleLoadError
    in
    Task.map2 initialModel loadData getViewport


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.browser of
        Nothing ->
            case m.viewType of
                Tiles _ ->
                    Sub.batch [ onResize Resize, Ports.scroll Scroll, Ports.lastEltVisible Scroll ]

                _ ->
                    Sub.none

        Just _ ->
            Sub.map StoryViewMsg StoryView.subscriptions


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Session )
update session msg model =
    let
        cache =
            Session.getCache session
    in
    case msg of
        StoryFilterInput f ->
            ( ( { model | storyFilter = f, stories = filterStories f cache.stories }
              , Cmd.none
              )
            , session
            )

        SetTableState t ->
            ( ( { model | tableState = t }, Cmd.none ), session )

        BrowseFrom storyId ->
            ( ( { model | browser = zipperFrom storyId model.stories }, Cmd.none ), session )

        StoryViewMsg svm ->
            let
                ( newStoryView, cmd ) =
                    StoryView.update svm model.storyView
            in
            ( ( { model | storyView = newStoryView }, Cmd.map StoryViewMsg cmd ), session )

        Next ->
            ( ( { model | browser = model.browser |> Maybe.map Zipper.next }, Cmd.none ), session )

        Previous ->
            ( ( { model | browser = model.browser |> Maybe.map Zipper.previous }, Cmd.none ), session )

        Scroll atBottom ->
            if atBottom then
                ( loadMore model, session )

            else
                ( ( model, Cmd.none ), session )

        Resize w h ->
            ( ( { model | windowSize = ( w, h ) }, Cmd.none ), session )

        CloseBrowser ->
            ( ( { model | browser = Nothing }, Cmd.none ), session )

        ToggleShowDisabledOnly ->
            let
                flag =
                    not model.showDisabledStoriesOnly

                newStories =
                    if flag then
                        List.filter (\s -> not s.enabled) model.stories

                    else
                        filterStories model.storyFilter cache.stories
            in
            ( ( { model | showDisabledStoriesOnly = flag, stories = newStories }, Cmd.none ), session )

        SelectStory s ->
            if isStudent session then
                saveWorkQueue model session [ s ]

            else
                ( ( model, Cmd.none ), updateCache (\c -> { c | selectedStories = s :: c.selectedStories }) session )

        ClearSelectedStories ->
            ( ( model, Cmd.none ), updateCache (\c -> { c | selectedStories = [] }) session )

        CreateAnthology ->
            ( ( { model | anthologyForm = Just (AnthologyForm "" "") }, Cmd.none ), session )

        SetAnthologyName n ->
            case model.anthologyForm of
                Just f ->
                    ( ( { model | anthologyForm = Just { name = n, description = f.description } }, Cmd.none ), session )

                Nothing ->
                    ( ( model, Cmd.none ), session )

        SetAnthologyDescription d ->
            case model.anthologyForm of
                Just f ->
                    ( ( { model | anthologyForm = Just { name = f.name, description = d } }, Cmd.none ), session )

                Nothing ->
                    ( ( model, Cmd.none ), session )

        SubmitAnthologyForm ->
            case model.anthologyForm of
                Just f ->
                    case validateAnthologyForm f of
                        [] ->
                            ( ( { model | errors = [], anthologyForm = Nothing }
                              , Api.postAnthologies (authorization session) (Api.Anthology "" f.name f.description "" (Just "") (List.map .id cache.selectedStories) False)
                                    |> Http.send CreateAnthologyResponse
                              )
                            , session
                            )

                        errors ->
                            ( ( { model | errors = errors }, Cmd.none ), session )

                Nothing ->
                    ( ( model, Cmd.none ), session )

        CreateAnthologyResponse (Ok newAnthology) ->
            ( ( { model | anthologyForm = Nothing, viewType = Anthologies }
              , Cmd.none
              )
            , updateAnthologies (\anthologies -> newAnthology :: anthologies) session
                |> updateCache (\c -> { c | selectedStories = [] })
                |> Session.success "New anthology created."
            )

        CreateAnthologyResponse (Err e) ->
            ( ( model, Cmd.none ), Session.error ("Couldn't create the anthology: " ++ defaultHttpErrorMsg e) session )

        SetViewType v ->
            ( ( { model | viewType = v }, Cmd.none ), session )

        DeleteAnthology aid ->
            ( ( model
              , Api.deleteAnthologiesByAnthologyId (authorization session) aid
                    |> Http.send DeleteAnthologyResponse
              )
            , session
            )

        DeleteAnthologyResponse (Ok aid) ->
            ( ( model, Cmd.none )
            , updateAnthologies (List.filter (\a -> a.id /= aid)) session
                |> Session.success "Anthology deleted."
            )

        DeleteAnthologyResponse (Err e) ->
            ( ( model, Cmd.none )
            , Session.error ("Couldn't delete the anthology: " ++ defaultHttpErrorMsg e) session
            )

        SetStarterStories aid ->
            ( ( model
              , Api.postAnthologiesByAnthologyIdStarter_stories (authorization session) aid
                    |> Http.send SetStarterStoriesResponse
              )
            , session
            )

        SetStarterStoriesResponse (Ok _) ->
            ( ( model, Cmd.none ), session )

        SetStarterStoriesResponse (Err e) ->
            ( ( model, Cmd.none )
            , Session.error ("Couldn't set the starter stories: " ++ defaultHttpErrorMsg e) session
            )

        UpdateAnthology a ->
            ( ( model
              , Api.postAnthologiesByAnthologyId (authorization session) a.id a
                    |> Http.send UpdateAnthologyResponse
              )
            , session
            )

        UpdateAnthologyResponse (Ok newA) ->
            ( ( model, Cmd.none )
            , updateAnthologies
                (List.map
                    (\a ->
                        if a.id == newA.id then
                            newA

                        else
                            a
                    )
                )
                session
            )

        UpdateAnthologyResponse (Err e) ->
            ( ( { model | errors = [ "Couldn't update the anthology: " ++ defaultHttpErrorMsg e ] }, Cmd.none ), session )

        AddAnthologyToWorkQueue stories ->
            saveWorkQueue model session stories

        SaveWorkQueueResponse (Ok _) ->
            ( ( model, Cmd.none ), Session.success "Updated work queue" session )

        SaveWorkQueueResponse (Err e) ->
            ( ( model, Cmd.none ), Session.error ("Couldn't save work queue: " ++ defaultHttpErrorMsg e) session )


saveWorkQueue : Model -> Session -> List Api.Story -> ( ( Model, Cmd Msg ), Session )
saveWorkQueue model session stories =
    Session.addToWorkQueue stories session
        |> Session.saveWorkQueue SaveWorkQueueResponse
        |> (\( cmd, newSession ) -> ( ( model, cmd ), newSession ))


updateAnthologies : (List Api.Anthology -> List Api.Anthology) -> Session -> Session
updateAnthologies f =
    Session.updateCache (\c -> { c | anthologies = f c.anthologies })


type alias Error =
    String


validateAnthologyForm : AnthologyForm -> List Error
validateAnthologyForm f =
    if String.length f.name <= 3 then
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
                ( { m | viewType = Tiles (n + (4 * StoryTiles.tilesPerRow (first m.windowSize))) }
                , Ports.isLastEltVisible StoryTiles.divId
                )

        _ ->
            ( m, Cmd.none )


zipperFrom : Int -> List Api.Story -> Maybe (Zipper Api.Story)
zipperFrom storyId stories =
    Zipper.fromList stories
        |> Maybe.andThen (Zipper.findFirst (\s -> s.id == storyId))


view : Session -> Model -> { title : String, content : Html Msg }
view session m =
    let
        cache =
            Session.getCache session
    in
    { title = "Find a Story"
    , content =
        div [ class "max-w-lg mx-auto px-2" ]
            [ case m.browser of
                Nothing ->
                    div []
                        [ viewStoriesFilter session m
                        , Form.viewErrorMsgs m.errors
                        , viewUnless (Session.isStudent session) <| viewStoryBasket m cache.selectedStories
                        , viewUnless (Session.workQueueHasSpace session)
                            (p [ class "my-3" ]
                                [ text "Your work queue is full. Perhaps you should "
                                , a [ href (Route.routeToString Route.Home) ] [ text "complete some of the stories in it." ]
                                ]
                            )
                        , case m.viewType of
                            Tiles n ->
                                StoryTiles.view False (Just BrowseFrom) (List.take n m.stories)

                            Table ->
                                viewStoriesTable m

                            Anthologies ->
                                viewAnthologies session
                        ]

                Just b ->
                    div [ class "mb-12" ]
                        [ viewBrowserToolbar session (Zipper.current b) cache.selectedStories
                        , Html.map StoryViewMsg <| StoryView.view (Session.getSettings session) (Zipper.current b) m.storyView
                        ]
            ]
    }


viewBrowserToolbar : Session -> Api.Story -> List Api.Story -> Html Msg
viewBrowserToolbar session s selected =
    let
        cache =
            Session.getCache session

        mkBtn attrs txt =
            li [] [ a (class "block no-underline bg-transparent hover:bg-blue text-sm md:text-base text-center text-blue-dark font-semibold hover:text-white py-1 px-4 border border-blue hover:border-transparent rounded-full cursor-pointer" :: attrs) [ text txt ] ]
    in
    nav [ class "w-full mb-4" ]
        [ ul [ class "list-reset flex flex-wrap justify-between" ]
            [ mkBtn [ href "#", onClick Previous ] "Prev"
            , ul [ class "list-reset flex justify-between" ]
                [ viewIf (isEditor session) <| mkBtn [ href (Route.routeToString (Route.Editor s.id)) ] "Edit"
                , mkBtn [ href "#", onClick CloseBrowser ] "Back to stories"
                , mkBtn [ href (Route.routeToString (Route.Story s.id)) ] "View"
                , viewUnless (Session.isStudent session || List.member s selected) <| mkBtn [ href "#", onClick (SelectStory s) ] "Add to basket"
                , viewIf
                    (Session.isStudent session
                        && not (List.member s (Session.getWorkQueue session))
                        && not (Dict.member s.id cache.answers)
                        && Session.workQueueHasSpace session
                    )
                    (mkBtn [ href "#", onClick (SelectStory s) ] "Add to work queue")
                ]
            , mkBtn [ href "#", onClick Next ] "Next"
            ]
        ]


viewStoriesFilter : Session -> Model -> Html Msg
viewStoriesFilter session m =
    let
        btn ( msg, txt ) =
            Components.btnSmall [ class "mr-1", type_ "button", onClick msg ] [ text txt ]
    in
    div [ class "flex items-center justify-between flex-wrap" ]
        [ div [ class "flex flex-wrap items-center" ]
            [ Form.input
                [ value m.storyFilter
                , onInput StoryFilterInput
                , placeholder "Search text"
                , id "storyfilter"
                , class "mr-2 mb-2"
                ]
                []
            , label [ class "mr-2 mb-2", for "storyfilter" ]
                [ text (String.fromInt (List.length m.stories) ++ " matching stories ")
                ]
            ]
        , div [ class "flex items-center mb-2" ]
            [ viewIf (isEditor session) (btn (toggleDisabledStoriesOnly m))
            , btn (cycleDisplay (Session.getCache session) m)
            ]
        ]


cycleDisplay : Cache -> Model -> ( Msg, String )
cycleDisplay cache m =
    let
        viewTiles =
            ( SetViewType (Tiles (StoryTiles.tilesPerPage m.windowSize)), "Switch view (tiles)" )

        viewTable =
            ( SetViewType Table, "Switch view (table)" )
    in
    case m.viewType of
        Tiles _ ->
            if List.isEmpty cache.anthologies then
                viewTable

            else
                ( SetViewType Anthologies, "Switch view (anthologies)" )

        Anthologies ->
            viewTable

        Table ->
            viewTiles


toggleDisabledStoriesOnly : Model -> ( Msg, String )
toggleDisabledStoriesOnly m =
    if m.showDisabledStoriesOnly then
        ( ToggleShowDisabledOnly, "Show all stories" )

    else
        ( ToggleShowDisabledOnly, "Hide enabled stories" )


viewStoriesTable : Model -> Html Msg
viewStoriesTable m =
    div []
        [ Table.view tableConfig m.tableState m.stories ]


filterStories : String -> List Api.Story -> List Api.Story
filterStories storyFilter stories =
    if String.length storyFilter < 3 then
        stories

    else
        let
            r =
                Regex.fromStringWith { caseInsensitive = True, multiline = False } storyFilter
                    |> Maybe.withDefault Regex.never

            tagMatch tags =
                List.Extra.find (Regex.contains r) tags /= Nothing

            qualMatch q =
                Maybe.map (Regex.contains r) q == Just True

            match story =
                Regex.contains r story.title || qualMatch story.qualification || tagMatch story.tags || Regex.contains r story.content
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
                , viewData = \s -> Table.HtmlDetails [ class "text-center", style "width" "5em" ] [ Html.text (String.fromInt s.level) ]
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
        { toId = String.fromInt << .id
        , toMsg = SetTableState
        , columns =
            [ storyTitleColumn
            , Table.stringColumn "General" (tag 1)
            , Table.stringColumn "BGE" (Maybe.withDefault "" << .curriculum)
            , Table.stringColumn "SQA" (Maybe.withDefault "" << .qualification)
            , levelColumn
            ]
        , customizations = Bootstrap.tableCustomizations
        }


viewStoryLink : Api.Story -> Html Msg
viewStoryLink s =
    Components.link [ href "#", onClick (BrowseFrom s.id) ] s.title


viewStoryBasket : Model -> List Api.Story -> Html Msg
viewStoryBasket m stories =
    let
        isEmpty =
            List.isEmpty stories

        createAnthology =
            case m.anthologyForm of
                Nothing ->
                    Components.btn [ class "text-sm", onClick CreateAnthology ] [ text "Create Anthology" ]

                Just f ->
                    Html.form [ class "flex flex-col md:flex-row", onSubmit SubmitAnthologyForm ]
                        [ Form.input
                            [ class "mb-2 md:mb-0 md:mr-2"
                            , placeholder "Anthology name"
                            , tabindex 1
                            , onInput SetAnthologyName
                            ]
                            []
                        , Form.input
                            [ class "mb-2 md:mb-0 md:mr-2"
                            , placeholder "Anthology description"
                            , tabindex 2
                            , onInput SetAnthologyDescription
                            ]
                            []
                        , Components.btn [ tabindex 3 ] [ text "Save anthology" ]
                        ]
    in
    Components.panel [ class "p-4 mb-4 relative" ]
        [ viewUnless isEmpty (span [ class "text-grey-dark" ] [ closeBtn ClearSelectedStories ])
        , h1 [ class "text-xl font-light mb-2" ] [ text "Story basket" ]
        , div [ id "storybasket" ]
            (if isEmpty then
                [ p [ title "Click on a story in the table to open the story browser" ] [ text "Your story basket is empty. You can browse the search results by clicking on a story title in the table, add stories to the basket, then use them to create an anthology." ]
                ]

             else
                [ StoryTiles.view True Nothing stories
                , createAnthology
                ]
            )
        ]


viewAnthologies : Session -> Html Msg
viewAnthologies session =
    let
        cache =
            Session.getCache session

        btn msg disable txt =
            Components.btnBase [ class "text-xs bg-blue py-1 px-2 mr-1", classList [ ( "hover:bg-blue-dark", not disable ), ( "opacity-50 cursor-not-allowed", disable ) ], disabled disable, onClick msg ] [ text txt ]

        anthologiesWithStories =
            List.map pickStories cache.anthologies

        pickStories a =
            ( a, List.filterMap (findStoryById cache) a.stories )

        canDelete a =
            (a.schoolId /= Nothing && Session.isTeacher session) || Session.isEditor session

        render ( a, astories ) =
            Components.panel [ class "p-4 mb-4" ]
                [ h1 [ class "text-xl font-light mb-1" ] [ text a.name ]
                , p [ class "text-sm text-grey-dark mb-1" ] [ text a.description ]
                , div [ class "mb-2" ]
                    [ viewIf (canDelete a)
                        (btn (DeleteAnthology a.id) False "Delete")
                    , viewIf (isEditor session)
                        (btn (UpdateAnthology { a | hidden = not a.hidden })
                            False
                            (if a.hidden then
                                "Un-hide"

                             else
                                "Hide"
                            )
                        )
                    , viewIf (isEditor session)
                        (btn (SetStarterStories a.id) (List.length a.stories < 24) "Set Starter Stories")
                    , viewIf (isStudent session && Session.workQueueHasSpace session)
                        (btn (AddAnthologyToWorkQueue astories) False "Add to my work queue")
                    ]
                , StoryTiles.view True Nothing astories
                ]
    in
    div [ class "anthologies" ]
        (List.map render anthologiesWithStories)
