module Main exposing (main)

import Data.Session as Session exposing (Session, Role(Teacher), User, decodeSession, storeSession)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.Class as Class
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
    | Class Class.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page


type alias Model =
    { session : Session
    , pageState : PageState
    }


init : Value -> Location -> ( Model, Cmd Msg )
init value location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded Blank
        , session = decodeSession value
        }


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session.user

        mapMsg m =
            Html.map (PageMsg << m)
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
                    |> mapMsg StoryMsg

            FindStory subModel ->
                FindStory.view session subModel
                    |> frame Page.FindStory
                    |> mapMsg FindStoryMsg

            Login subModel ->
                Login.view session subModel
                    |> frame Page.Other
                    |> mapMsg LoginMsg

            Students subModel ->
                Students.view session subModel
                    |> frame Page.Teacher
                    |> mapMsg StudentsMsg

            Student subModel ->
                Student.view subModel
                    |> frame Page.Teacher
                    |> mapMsg StudentMsg

            Classes subModel ->
                Classes.view session subModel
                    |> frame Page.Teacher
                    |> mapMsg ClassesMsg

            Class subModel ->
                Class.view session subModel
                    |> frame Page.Teacher
                    |> mapMsg ClassMsg


type Msg
    = SetRoute (Maybe Route)
    | PageLoaded PageLoaded
    | PageMsg PageMsg


type PageLoaded
    = HomeLoaded (Result PageLoadError Session)
    | StoryLoaded (Result PageLoadError ( Story.Model, Session ))
    | FindStoryLoaded (Result PageLoadError ( FindStory.Model, Session ))
    | StudentsLoaded (Result PageLoadError ( Students.Model, Session ))
    | StudentLoaded (Result PageLoadError ( Student.Model, Session ))
    | ClassesLoaded (Result PageLoadError ( Classes.Model, Session ))
    | ClassLoaded (Result PageLoadError ( Class.Model, Session ))


type PageMsg
    = StoryMsg Story.Msg
    | LoginMsg Login.Msg
    | FindStoryMsg FindStory.Msg
    | StudentsMsg Students.Msg
    | StudentMsg Student.Msg
    | ClassesMsg Classes.Msg
    | ClassMsg Class.Msg


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Task.attempt (PageLoaded << toMsg) task

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

                Route.Class slug ->
                    transition ClassLoaded (Class.init session slug)
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
                let
                    s =
                        { session | user = Nothing }
                in
                    { model | session = s }
                        => Cmd.batch [ storeSession s, Route.modifyUrl Route.Home ]

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute route ->
            setRoute route model

        PageLoaded p ->
            pageLoaded p model

        PageMsg p ->
            updatePage (getPage model.pageState) p model


pageLoaded : PageLoaded -> Model -> ( Model, Cmd Msg )
pageLoaded msg model =
    case msg of
        StoryLoaded (Ok ( subModel, newSession )) ->
            { model | session = newSession, pageState = Loaded (Story subModel) }
                => Ports.postProcessStory (.words subModel.story)

        StoryLoaded (Err error) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        HomeLoaded (Ok newSession) ->
            { model | session = newSession, pageState = Loaded Home } => Cmd.none

        HomeLoaded (Err error) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        FindStoryLoaded (Ok ( subModel, newSession )) ->
            { model | session = newSession, pageState = Loaded (FindStory subModel) } => Cmd.none

        FindStoryLoaded (Err error) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        StudentsLoaded (Ok ( subModel, newSession )) ->
            { model | session = newSession, pageState = Loaded (Students subModel) } => Cmd.none

        StudentsLoaded (Err error) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        ClassesLoaded (Ok ( subModel, newSession )) ->
            { model | session = newSession, pageState = Loaded (Classes subModel) } => Cmd.none

        ClassesLoaded (Err error) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        ClassLoaded (Ok ( subModel, newSession )) ->
            { model | session = newSession, pageState = Loaded (Class subModel) } => Cmd.none

        ClassLoaded (Err error) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        StudentLoaded (Ok ( subModel, newSession )) ->
            { model | session = newSession, pageState = Loaded (Student subModel) } => Cmd.none

        StudentLoaded (Err error) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none


updatePage : Page -> PageMsg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, mapMsg toMsg newCmd )

        mapMsg m =
            Cmd.map (PageMsg << m)
    in
        case ( msg, page ) of
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
                        => mapMsg StudentsMsg cmd

            ( StudentMsg subMsg, Student subModel ) ->
                toPage Student StudentMsg (Student.update model.session) subMsg subModel

            ( ClassesMsg subMsg, Classes subModel ) ->
                let
                    ( ( pageModel, cmd ), newSession ) =
                        Classes.update model.session subMsg subModel
                in
                    { model | session = newSession, pageState = Loaded (Classes pageModel) }
                        => mapMsg ClassesMsg cmd

            ( ClassMsg subMsg, Class subModel ) ->
                let
                    ( ( pageModel, cmd ), externalMsg ) =
                        Class.update model.session subMsg subModel
                in
                    case externalMsg of
                        Class.NoOp ->
                            { model | pageState = Loaded (Class pageModel) } => mapMsg ClassMsg cmd

                        Class.Deleted newSession ->
                            setRoute (Just (Route.Teacher Route.Classes))
                                { model | session = newSession }

                        Class.Updated newSession ->
                            { model | pageState = Loaded (Class pageModel), session = newSession }
                                => mapMsg ClassMsg cmd

            ( LoginMsg subMsg, Login subModel ) ->
                let
                    ( ( pageModel, loginCmd ), maybeLoggedIn ) =
                        Login.update subMsg subModel

                    ( newSession, cmd ) =
                        maybeLoggedIn
                            |> Maybe.map (Session.newLogin model.session)
                            |> Maybe.map (\s -> ( s, Cmd.batch [ storeSession s, chooseStartPage s.user ] ))
                            |> Maybe.withDefault ( model.session, Cmd.none )
                in
                    { model | session = newSession, pageState = Loaded (Login pageModel) }
                        => Cmd.batch [ mapMsg LoginMsg loginCmd, cmd ]

            ( _, _ ) ->
                model => Cmd.none


chooseStartPage : Maybe User -> Cmd msg
chooseStartPage user =
    Route.modifyUrl <|
        case Maybe.map .role user of
            Just Teacher ->
                Route.Teacher Route.Students

            _ ->
                Route.Home


subscriptions : Model -> Sub Msg
subscriptions m =
    pageSubscriptions (getPage m.pageState)
        |> Sub.map PageMsg


pageSubscriptions : Page -> Sub PageMsg
pageSubscriptions page =
    case page of
        Story _ ->
            Sub.map StoryMsg Story.subscriptions

        _ ->
            Sub.none


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
