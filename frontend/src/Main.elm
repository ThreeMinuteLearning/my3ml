module Main exposing (main)

import Data.Session as Session exposing (Session, Role)
import Html exposing (..)
import Navigation exposing (Location)
import Page.Classes as Classes
import Page.Errored as Errored exposing (PageLoadError)
import Page.FindStory as FindStory
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Story as Story
import Page.Student as Student
import Page.Students as Students
import Ports
import Route exposing (Route)
import Task
import Util exposing ((=>))
import Views.Page as Page exposing (ActivePage)


type Page
    = Blank
    | NotFound
    | Home
    | Errored PageLoadError
    | Login Login.Model
    | Story Story.Model
    | FindStory FindStory.Model
    | Students Students.Model
    | Student Student.Model
    | Classes Classes.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page


type alias Model =
    { session : Session
    , pageState : PageState
    }


init : Location -> ( Model, Cmd MsgNew )
init location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded Blank
        , session = Session.emptySession
        }


view : Model -> Html MsgNew
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html MsgNew
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session.user
    in
        case page of
            NotFound ->
                NotFound.view session
                    |> frame Page.Other

            Blank ->
                Html.text ""
                    |> frame Page.Other

            Home ->
                Home.view session
                    |> frame Page.Home

            Errored subModel ->
                Errored.view session subModel
                    |> frame Page.Other

            Story subModel ->
                Story.view session subModel
                    |> frame Page.Home
                    |> Html.map StoryMsg

            FindStory subModel ->
                FindStory.view session subModel
                    |> frame Page.FindStory
                    |> Html.map FindStoryMsg

            Login subModel ->
                Login.view session subModel
                    |> frame Page.Other
                    |> Html.map LoginMsg

            Students subModel ->
                Students.view session subModel
                    |> frame Page.Teacher
                    |> Html.map StudentsMsg

            Student subModel ->
                Student.view subModel
                    |> frame Page.Teacher
                    |> Html.map StudentMsg

            Classes subModel ->
                Classes.view session subModel
                    |> frame Page.Teacher
                    |> Html.map ClassesMsg


type MsgNew
    = SetRoute (Maybe Route)
    | HomeLoaded (Result PageLoadError Session)
    | StoryLoaded (Result PageLoadError ( Story.Model, Session ))
    | FindStoryLoaded (Result PageLoadError ( FindStory.Model, Session ))
    | StudentsLoaded (Result PageLoadError ( Students.Model, Session ))
    | StudentLoaded (Result PageLoadError ( Student.Model, Session ))
    | ClassesLoaded (Result PageLoadError ( Classes.Model, Session ))
    | StoryMsg Story.Msg
    | LoginMsg Login.Msg
    | FindStoryMsg FindStory.Msg
    | StudentsMsg Students.Msg
    | StudentMsg Student.Msg
    | ClassesMsg Classes.Msg


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


setRoute : Maybe Route -> Model -> ( Model, Cmd MsgNew )
setRoute maybeRoute model =
    let
        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Task.attempt toMsg task

        errored =
            pageErrored model

        session =
            model.session

        requireRole role page transition_ =
            case session.user of
                Nothing ->
                    errored page "You are signed out. You need to sign-in to view this page."

                Just u ->
                    if u.role == role then
                        transition_
                    else
                        errored page "You can't view this page as the current use. Perhaps you need to log in as a teacher?"

        teacherRoute subRoute =
            case subRoute of
                Route.Students ->
                    transition StudentsLoaded (Students.init session)

                Route.Classes ->
                    transition ClassesLoaded (Classes.init session)

                Route.Student slug ->
                    transition StudentLoaded (Student.init session slug)
    in
        case maybeRoute of
            Nothing ->
                { model | pageState = Loaded NotFound } => Cmd.none

            Just (Route.Home) ->
                transition HomeLoaded (Home.init session)

            Just (Route.Story slug) ->
                transition StoryLoaded (Story.init session slug)

            Just (Route.Login) ->
                { model | pageState = Loaded (Login Login.initialModel) } => Cmd.none

            Just (Route.Logout) ->
                { model | session = { session | user = Nothing } }
                    => Route.modifyUrl Route.Home

            Just (Route.FindStory) ->
                transition FindStoryLoaded (FindStory.init session)

            Just (Route.Teacher subRoute) ->
                requireRole Session.Teacher Page.Teacher (teacherRoute subRoute)

            _ ->
                Debug.log ("No route set for " ++ toString maybeRoute) (model ! [])


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
        { model | pageState = Loaded (Errored error) } => Cmd.none


update : MsgNew -> Model -> ( Model, Cmd MsgNew )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> MsgNew -> Model -> ( Model, Cmd MsgNew )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( StoryLoaded (Ok ( subModel, newSession )), _ ) ->
                { model | session = newSession, pageState = Loaded (Story subModel) } => Ports.postProcessStory (.words subModel.story)

            ( StoryLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( HomeLoaded (Ok newSession), _ ) ->
                { model | session = newSession, pageState = Loaded Home } => Cmd.none

            ( HomeLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( FindStoryLoaded (Ok ( subModel, newSession )), _ ) ->
                { model | session = newSession, pageState = Loaded (FindStory subModel) } => Cmd.none

            ( FindStoryLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( StudentsLoaded (Ok ( subModel, newSession )), _ ) ->
                { model | session = newSession, pageState = Loaded (Students subModel) } => Cmd.none

            ( StudentsLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( ClassesLoaded (Ok ( subModel, newSession )), _ ) ->
                { model | session = newSession, pageState = Loaded (Classes subModel) } => Cmd.none

            ( ClassesLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( StudentLoaded (Ok ( subModel, newSession )), _ ) ->
                { model | session = newSession, pageState = Loaded (Student subModel) } => Cmd.none

            ( StudentLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( StoryMsg subMsg, Story subModel ) ->
                toPage Story StoryMsg (Story.update model.session) subMsg subModel

            ( FindStoryMsg subMsg, FindStory subModel ) ->
                toPage FindStory FindStoryMsg (FindStory.update model.session) subMsg subModel

            ( StudentsMsg subMsg, Students subModel ) ->
                let
                    ( ( pageModel, cmd ), newSession ) =
                        Students.update model.session subMsg subModel
                in
                    { model | session = newSession, pageState = Loaded (Students pageModel) }
                        => Cmd.map StudentsMsg cmd

            ( StudentMsg subMsg, Student subModel ) ->
                toPage Student StudentMsg (Student.update model.session) subMsg subModel

            ( ClassesMsg subMsg, Classes subModel ) ->
                let
                    ( ( pageModel, cmd ), newSession ) =
                        Classes.update model.session subMsg subModel
                in
                    { model | session = newSession, pageState = Loaded (Classes pageModel) }
                        => Cmd.map ClassesMsg cmd

            ( LoginMsg subMsg, Login subModel ) ->
                let
                    ( ( pageModel, cmd ), msgFromPage ) =
                        Login.update subMsg subModel

                    newModel =
                        case msgFromPage of
                            Login.NoOp ->
                                model

                            Login.LoginSuccess login ->
                                { model | session = Session.newLogin login model.session }
                in
                    { newModel | pageState = Loaded (Login pageModel) }
                        => Cmd.map LoginMsg cmd

            ( _, _ ) ->
                model => Cmd.none


subscriptions : Model -> Sub MsgNew
subscriptions m =
    pageSubscriptions (getPage m.pageState)


pageSubscriptions : Page -> Sub MsgNew
pageSubscriptions page =
    case page of
        Story _ ->
            Sub.map StoryMsg Story.subscriptions

        _ ->
            Sub.none


main : Program Never Model MsgNew
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
