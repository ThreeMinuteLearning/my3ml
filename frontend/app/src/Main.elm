module Main exposing (main)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Session as Session exposing (Session, decodeSession, storeSession)
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Value)
import Page.Account as Account
import Page.Class as Class
import Page.Classes as Classes
import Page.Editor as Editor
import Page.Errored as Errored exposing (PageLoadError(..))
import Page.FindStory as FindStory
import Page.Home as Home
import Page.LeaderBoard as LeaderBoard
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Register as Register
import Page.Story as Story
import Page.Student as Student
import Page.Students as Students
import Page.Teachers as Teachers
import Ports
import Route exposing (Route)
import Task
import Time
import Tuple exposing (second)
import Url exposing (Url)
import Views.Page as Page exposing (ActivePage)


type Page
    = Blank
    | NotFound
    | Home Home.Model
    | Errored PageLoadError
    | Login Login.Model
    | Story Story.Model
    | FindStory FindStory.Model
    | Students Students.Model
    | Student Student.Model
    | Classes Classes.Model
    | Class Class.Model
    | Editor Editor.Model
    | LeaderBoard LeaderBoard.Model
    | Account Account.Model
    | Register Register.Model
    | Teachers Teachers.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page


type alias Model =
    { session : Session
    , pageState : PageState
    , navKey : Nav.Key
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init value url navKey =
    changeRouteTo (Route.fromUrl url)
        { pageState = Loaded Blank
        , session = decodeSession value
        , navKey = navKey
        }


view : Model -> Document Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Document Msg
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session CloseAlert

        mapMsg m { title, content } =
            { title = title, content = Html.map (PageMsg << m) content }
    in
    case page of
        NotFound ->
            NotFound.view session
                |> frame Page.Other

        Blank ->
            { title = "", content = Html.text "" }
                |> frame Page.Other

        Home subModel ->
            Home.view session ClearWorkQueue subModel
                |> frame Page.Home

        Errored subModel ->
            Errored.view subModel
                |> frame Page.Other

        Story subModel ->
            Story.view session subModel
                |> mapMsg StoryMsg
                |> frame Page.Home

        FindStory subModel ->
            FindStory.view session subModel
                |> mapMsg FindStoryMsg
                |> frame Page.FindStory

        Login subModel ->
            { title = "Login to 3ml"
            , content = Html.map (PageMsg << LoginMsg) (Login.view subModel (Just (Route.href Route.Register)))
            }
                |> frame Page.Login

        Students subModel ->
            Students.view session subModel
                |> mapMsg StudentsMsg
                |> frame Page.Teacher

        Student subModel ->
            Student.view subModel
                |> mapMsg StudentMsg
                |> frame Page.Teacher

        Classes subModel ->
            Classes.view session subModel
                |> mapMsg ClassesMsg
                |> frame Page.Teacher

        Class subModel ->
            Class.view session subModel
                |> mapMsg ClassMsg
                |> frame Page.Teacher

        Editor subModel ->
            Editor.view subModel
                |> mapMsg EditorMsg
                |> frame Page.Other

        LeaderBoard subModel ->
            LeaderBoard.view subModel
                |> frame Page.LeaderBoard

        Account subModel ->
            Account.view subModel
                |> mapMsg AccountMsg
                |> frame Page.Account

        Register subModel ->
            Register.view subModel
                |> mapMsg RegisterMsg
                |> frame Page.Register

        Teachers subModel ->
            Teachers.view session subModel
                |> mapMsg TeachersMsg
                |> frame Page.Teacher


type Msg
    = SetRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | PageLoaded PageLoaded
    | PageMsg PageMsg
    | CloseAlert Session.Alert
    | ClearWorkQueue
    | SaveWorkQueueResponse (Result Http.Error Api.NoContent)
    | Tick Time.Posix


type PageLoaded
    = HomeLoaded (Result PageLoadError ( Home.Model, Session ))
    | StoryLoaded (Result PageLoadError ( Story.Model, Session ))
    | FindStoryLoaded (Result PageLoadError ( FindStory.Model, Session ))
    | StudentsLoaded (Result PageLoadError ( Students.Model, Session ))
    | TeachersLoaded (Result PageLoadError Teachers.Model)
    | StudentLoaded (Result PageLoadError ( Student.Model, Session ))
    | ClassesLoaded (Result PageLoadError ( Classes.Model, Session ))
    | ClassLoaded (Result PageLoadError ( Class.Model, Session ))
    | EditorLoaded (Result PageLoadError ( Editor.Model, Session ))
    | AccountLoaded (Result PageLoadError ( Account.Model, Session ))
    | LeaderBoardLoaded (Result PageLoadError LeaderBoard.Model)


type PageMsg
    = StoryMsg Story.Msg
    | LoginMsg (Login.Msg Api.Login)
    | FindStoryMsg FindStory.Msg
    | StudentsMsg Students.Msg
    | StudentMsg Student.Msg
    | ClassesMsg Classes.Msg
    | ClassMsg Class.Msg
    | EditorMsg Editor.Msg
    | AccountMsg Account.Msg
    | RegisterMsg Register.Msg
    | TeachersMsg Teachers.Msg


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        transition toMsg task =
            ( { model | pageState = TransitioningFrom (getPage model.pageState) }
            , Task.attempt (PageLoaded << toMsg) task
            )

        errored =
            pageErrored model

        session =
            model.session

        requireRole hasRole transition_ =
            case Session.subjectId session of
                Nothing ->
                    errored "You are signed out. You need to sign-in to view this page."

                _ ->
                    if hasRole session then
                        transition_

                    else
                        errored "You can't view this page as the current user. Perhaps you need to log in as a teacher?"

        teacherRoute subRoute =
            case subRoute of
                Route.Students ->
                    transition StudentsLoaded (Students.init session)

                Route.Classes ->
                    transition ClassesLoaded (Classes.init session)

                Route.Teachers ->
                    transition TeachersLoaded (Teachers.init session)

                Route.Student slug ->
                    transition StudentLoaded (Student.init session slug)

                Route.Class slug ->
                    transition ClassLoaded (Class.init session slug)
    in
    case maybeRoute of
        Nothing ->
            ( { model | pageState = Loaded NotFound }, Cmd.none )

        Just Route.Home ->
            transition HomeLoaded (Home.init session)

        Just (Route.Story slug) ->
            transition StoryLoaded (Story.init session slug)

        Just (Route.Editor slug) ->
            requireRole Session.isEditor <|
                transition EditorLoaded (Editor.init session slug)

        Just Route.Login ->
            ( { model | pageState = Loaded (Login Login.initialModel) }, Cmd.none )

        Just Route.Logout ->
            let
                s =
                    Session.logout session
            in
            ( { model | session = s }
            , Cmd.batch [ storeSession s, Route.modifyUrl model.navKey Route.Login ]
            )

        Just Route.FindStory ->
            transition FindStoryLoaded (FindStory.init session)

        Just (Route.Teacher subRoute) ->
            requireRole Session.isTeacher (teacherRoute subRoute)

        Just Route.LeaderBoard ->
            transition LeaderBoardLoaded (LeaderBoard.init session)

        Just Route.Account ->
            transition AccountLoaded (Account.init session)

        Just Route.Register ->
            ( { model | pageState = Loaded (Register Register.init) }, Cmd.none )

        Just Route.Trails ->
            ( model, Cmd.none )


pageErrored : Model -> String -> ( Model, Cmd msg )
pageErrored model errorMessage =
    ( { model | pageState = Loaded (Errored (PageLoadError errorMessage)) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute route ->
            changeRouteTo route model

        ChangedUrl url ->
            changeRouteTo (Route.fromUrl url) model

        ClickedLink urlReq ->
            case urlReq of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- Special case where we want to navigate away from the app to the static site
                            if url.path == "/" then
                                ( model, Nav.load "/" )

                            else
                                ( model, Cmd.none )

                        Just "" ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        PageLoaded p ->
            pageLoaded p model

        PageMsg p ->
            updatePage (getPage model.pageState) p model

        CloseAlert a ->
            ( { model | session = Session.closeAlert a model.session }, Cmd.none )

        Tick t ->
            ( { model | session = Session.tick model.session t }, Cmd.none )

        ClearWorkQueue ->
            let
                ( cmd, newSession ) =
                    Session.clearWorkQueue model.session
                        |> Session.saveWorkQueue SaveWorkQueueResponse
            in
            ( { model | session = newSession }, cmd )

        SaveWorkQueueResponse _ ->
            ( model, Cmd.none )


pageLoaded : PageLoaded -> Model -> ( Model, Cmd Msg )
pageLoaded msg model =
    let
        handlePageLoadError result f =
            case result of
                Ok a ->
                    f a

                Err AuthenticationRequired ->
                    ( { model | session = Session.logout model.session, pageState = Loaded (Errored AuthenticationRequired) }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        pageLoadedWithNewSession r toModel =
            handlePageLoadError r <|
                \( subModel, newSession ) ->
                    ( { model | session = newSession, pageState = Loaded (toModel subModel) }, Task.perform Tick Time.now )
    in
    case msg of
        StoryLoaded r ->
            handlePageLoadError r <|
                \( subModel, newSession ) ->
                    ( { model | session = newSession, pageState = Loaded (Story subModel) }
                    , Ports.postProcessStory (.words subModel.story)
                    )

        HomeLoaded r ->
            pageLoadedWithNewSession r Home

        EditorLoaded r ->
            pageLoadedWithNewSession r Editor

        FindStoryLoaded r ->
            pageLoadedWithNewSession r FindStory

        StudentsLoaded r ->
            pageLoadedWithNewSession r Students

        StudentLoaded r ->
            pageLoadedWithNewSession r Student

        ClassesLoaded r ->
            pageLoadedWithNewSession r Classes

        ClassLoaded r ->
            pageLoadedWithNewSession r Class

        LeaderBoardLoaded r ->
            handlePageLoadError r <|
                \subModel ->
                    ( { model | pageState = Loaded (LeaderBoard subModel) }, Cmd.none )

        AccountLoaded r ->
            pageLoadedWithNewSession r Account

        TeachersLoaded r ->
            handlePageLoadError r <|
                \subModel ->
                    ( { model | pageState = Loaded (Teachers subModel) }, Cmd.none )


updatePage : Page -> PageMsg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | pageState = Loaded (toModel newModel) }, mapMsg toMsg newCmd )

        toPageUpdateSession toModel toMsg subUpdate subMsg subModel =
            let
                ( ( newModel, newCmd ), newSession ) =
                    subUpdate model.session subMsg subModel
            in
            ( { model | session = newSession, pageState = Loaded (toModel newModel) }, mapMsg toMsg newCmd )

        mapMsg m =
            Cmd.map (PageMsg << m)
    in
    case ( msg, page ) of
        ( StoryMsg subMsg, Story subModel ) ->
            toPageUpdateSession Story StoryMsg Story.update subMsg subModel

        ( FindStoryMsg subMsg, FindStory subModel ) ->
            toPageUpdateSession FindStory FindStoryMsg FindStory.update subMsg subModel

        ( StudentsMsg subMsg, Students subModel ) ->
            toPageUpdateSession Students StudentsMsg Students.update subMsg subModel

        ( StudentMsg subMsg, Student subModel ) ->
            toPageUpdateSession Student StudentMsg Student.update subMsg subModel

        ( ClassesMsg subMsg, Classes subModel ) ->
            toPageUpdateSession Classes ClassesMsg Classes.update subMsg subModel

        ( ClassMsg subMsg, Class subModel ) ->
            let
                ( ( pageModel, cmd ), externalMsg ) =
                    Class.update model.session subMsg subModel
            in
            case externalMsg of
                Class.NoOp ->
                    ( { model | pageState = Loaded (Class pageModel) }, mapMsg ClassMsg cmd )

                Class.Deleted newSession ->
                    changeRouteTo (Just (Route.Teacher Route.Classes))
                        { model | session = newSession }

                Class.Updated newSession ->
                    ( { model | pageState = Loaded (Class pageModel), session = newSession }
                    , mapMsg ClassMsg cmd
                    )

        ( LoginMsg subMsg, Login subModel ) ->
            let
                loginRequest username password otp =
                    Api.postAuthenticate (Api.LoginRequest username password otp (Session.userAgent model.session))

                ( ( pageModel, loginCmd ), maybeLoggedIn ) =
                    Login.update subMsg subModel loginRequest

                ( newSession, cmd ) =
                    maybeLoggedIn
                        |> Maybe.map (Session.newLogin model.session)
                        |> Maybe.map (\s -> ( s, Cmd.batch [ storeSession s, chooseStartPage model.navKey s ] ))
                        |> Maybe.withDefault ( model.session, Cmd.none )
            in
            ( { model | session = newSession, pageState = Loaded (Login pageModel) }
            , Cmd.batch [ mapMsg LoginMsg loginCmd, cmd ]
            )

        ( EditorMsg subMsg, Editor subModel ) ->
            toPage Editor EditorMsg (Editor.update model.session) subMsg subModel

        ( AccountMsg subMsg, Account subModel ) ->
            toPageUpdateSession Account AccountMsg Account.update subMsg subModel

        ( RegisterMsg subMsg, Register subModel ) ->
            toPage Register RegisterMsg (Register.update model.session) subMsg subModel

        ( TeachersMsg subMsg, Teachers subModel ) ->
            toPage Teachers TeachersMsg (Teachers.update model.session) subMsg subModel

        ( _, _ ) ->
            ( model, Cmd.none )


chooseStartPage : Nav.Key -> Session -> Cmd msg
chooseStartPage navKey session =
    Route.modifyUrl navKey <|
        if Session.isTeacher session then
            Route.Teacher Route.Students

        else
            Route.Home


subscriptions : Model -> Sub Msg
subscriptions m =
    pageSubscriptions (getPage m.pageState)
        |> Sub.map PageMsg
        |> (\s -> Sub.batch [ s, sessionSubscriptions m.session ])


sessionSubscriptions : Session -> Sub Msg
sessionSubscriptions s =
    case Session.getAlerts s of
        [] ->
            Sub.none

        _ ->
            Time.every 1000 Tick


pageSubscriptions : Page -> Sub PageMsg
pageSubscriptions page =
    case page of
        Story _ ->
            Sub.map StoryMsg Story.subscriptions

        Editor subModel ->
            Sub.map EditorMsg (Editor.subscriptions subModel)

        FindStory subModel ->
            Sub.map FindStoryMsg (FindStory.subscriptions subModel)

        _ ->
            Sub.none


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
