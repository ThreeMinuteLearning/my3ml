module Main exposing (main)

import Data.Session exposing (Session, newLogin)
import Dict
import Html exposing (..)
import Navigation exposing (Location)
import Page.Errored as Errored exposing (PageLoadError)
import Page.FindStory as FindStory
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Story as Story
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
        , session = { user = Nothing, stories = [], dict = Dict.empty }
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


type MsgNew
    = SetRoute (Maybe Route)
    | HomeLoaded (Result PageLoadError Session)
    | StoryLoaded (Result PageLoadError ( Story.Model, Session ))
    | FindStoryLoaded (Result PageLoadError ( FindStory.Model, Session ))
    | StoryMsg Story.Msg
    | LoginMsg Login.Msg
    | FindStoryMsg FindStory.Msg
    | PrintWindow


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
    in
        case maybeRoute of
            Nothing ->
                { model | pageState = Loaded NotFound } => Cmd.none

            Just (Route.Home) ->
                transition HomeLoaded (Home.init model.session)

            Just (Route.Story slug) ->
                transition StoryLoaded (Story.init model.session slug)

            Just (Route.Login) ->
                { model | pageState = Loaded (Login Login.initialModel) } => Cmd.none

            Just (Route.Logout) ->
                let
                    session =
                        model.session
                in
                    { model | session = { session | user = Nothing } }
                        => Route.modifyUrl Route.Home

            Just (Route.FindStory) ->
                transition FindStoryLoaded (FindStory.init model.session)

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

            ( StoryMsg subMsg, Story subModel ) ->
                toPage Story StoryMsg (Story.update model.session) subMsg subModel

            ( FindStoryMsg subMsg, FindStory subModel ) ->
                toPage FindStory FindStoryMsg (FindStory.update model.session) subMsg subModel

            ( LoginMsg subMsg, Login subModel ) ->
                let
                    ( ( pageModel, cmd ), msgFromPage ) =
                        Login.update subMsg subModel

                    newModel =
                        case msgFromPage of
                            Login.NoOp ->
                                model

                            Login.LoginSuccess login ->
                                { model | session = newLogin login model.session }
                in
                    { newModel | pageState = Loaded (Login pageModel) }
                        => Cmd.map LoginMsg cmd

            ( PrintWindow, _ ) ->
                model => Ports.printWindow ()

            ( _, _ ) ->
                model => Cmd.none



{-

   initSchoolData : SchoolData
   initSchoolData =
       { classes = RemoteData.Loading
       , students = RemoteData.Loading
       , tableState = Table.initialSort "Name"
       , action = ViewStudents
       , addStudentsForm = AddStudentsForm.init
       , studentFilter = ( "", Nothing )
       , selectedStudents = Dict.empty
       , addClassForm = AddClassForm.init
       , studentAccountsCreated = []
       }
-}
{-
   updateSchoolData : User -> SchoolDataMsg -> SchoolData -> ( SchoolData, Cmd Msg )
   updateSchoolData (User _ token) msg sd =
       case msg of
           ClassesResponse cs ->
               { sd | classes = cs } ! []

           StudentsResponse ss ->
               { sd | students = ss } ! []

           SchoolDataTableState ts ->
               { sd | tableState = ts } ! []

           TeacherAction ta ->
               { sd | action = ta } ! []

           StudentFilterInput f ->
               { sd | studentFilter = ( f, second sd.studentFilter ) } ! []

           StudentFilterClass c ->
               { sd | studentFilter = ( first sd.studentFilter, c ) } ! []

           ClearSelectedStudents ->
               { sd | selectedStudents = Dict.empty } ! []

           ClearNewAccounts ->
               { sd | studentAccountsCreated = [] } ! []

           AddClassFormMsg formMsg ->
               case ( formMsg, Form.getOutput sd.addClassForm ) of
                   ( Form.Submit, Just newClass ) ->
                       { sd | action = ViewClasses, addClassForm = AddClassForm.init } ! [ Rest.createClass token newClass ]

                   _ ->
                       { sd | addClassForm = AddClassForm.update formMsg sd.addClassForm } ! []

           AddStudentsFormMsg formMsg ->
               case ( formMsg, Form.getOutput sd.addStudentsForm ) of
                   ( Form.Submit, Just newStudents ) ->
                       { sd | action = ViewStudents, addStudentsForm = AddStudentsForm.init } ! [ Rest.createStudentAccounts token (List.filter (not << String.isEmpty) newStudents) ]

                   _ ->
                       { sd | addStudentsForm = AddStudentsForm.update formMsg sd.addStudentsForm } ! []

           AddStudentsResponse r ->
               case r of
                   RemoteData.Success newAccounts ->
                       let
                           accountsCreated =
                               newAccounts ++ sd.studentAccountsCreated

                           newStudents =
                               List.map first newAccounts
                       in
                           { sd | studentAccountsCreated = accountsCreated, students = RemoteData.map (List.append newStudents) sd.students } ! []

                   -- TODO Post message on failure
                   _ ->
                       sd ! []

           AddClassResponse r ->
               case r of
                   RemoteData.Success newClass ->
                       { sd | classes = RemoteData.map ((::) newClass) sd.classes } ! []

                   -- TODO Post message on failure
                   _ ->
                       sd ! []

           SelectStudent s b ->
               let
                   f =
                       if b then
                           Dict.insert (.id s) s
                       else
                           Dict.remove (.id s)
               in
                   { sd | selectedStudents = f sd.selectedStudents } ! []


-}


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
