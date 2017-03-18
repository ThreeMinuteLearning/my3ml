module Main exposing (main)

import AddStudentsForm
import AnswersForm
import Api
import Drawer exposing (drawer)
import Html exposing (Html, div, img, h2, text, li)
import Html.Attributes exposing (id, class, href, src)
import Login
import Nav
import Navigation exposing (Location)
import RemoteData
import Rest
import Routing exposing (Page(..), locationToPage, pageToUrl)
import Stories
import Table
import Teacher
import Types exposing (..)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( page, cmd ) =
            authRedirect (locationToPage location) initMode

        initialModel =
            Model initStoryData page initMode
    in
        ( initialModel, Cmd.batch [ cmd, Rest.getDictionary, Rest.getStories emptyAccessToken ] )


emptyAccessToken : AccessToken
emptyAccessToken =
    AccessToken ""


initMode : AppMode
initMode =
    Anon (Login.initModel)


initStoryData : StoryData
initStoryData =
    { stories = RemoteData.Loading
    , storyFilter = ""
    , tableState = Table.initialSort "Title"
    , showDrawer = Nothing
    , answersForm = AnswersForm.init
    , wordDict = RemoteData.Loading
    }


initSchoolData : SchoolData
initSchoolData =
    { classes = RemoteData.Loading
    , students = RemoteData.Loading
    , tableState = Table.initialSort "Name"
    , action = ViewStudents
    , addStudentsForm = AddStudentsForm.init
    }


pageAllowed : Page -> AppMode -> Bool
pageAllowed page mode =
    case ( page, mode ) of
        ( HomePage, _ ) ->
            True

        ( StoryPage _, _ ) ->
            True

        ( LoginPage, Anon _ ) ->
            True

        ( _, Anon _ ) ->
            False

        ( LoginPage, _ ) ->
            False

        ( TeacherPage, TeacherMode _ _ ) ->
            True

        ( TeacherPage, _ ) ->
            False

        _ ->
            True


authRedirect : Page -> AppMode -> ( Page, Cmd Msg )
authRedirect page mode =
    if not <| pageAllowed page mode then
        ( HomePage, Navigation.modifyUrl <| pageToUrl HomePage )
    else
        ( page, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case ( msg, m.mode ) of
        ( ChangePage Logout, _ ) ->
            { m | storyData = initStoryData, page = HomePage, mode = initMode }
                ! [ Navigation.modifyUrl <| pageToUrl HomePage, Rest.getStories emptyAccessToken ]

        ( ChangePage page, mode ) ->
            let
                sd =
                    m.storyData

                ( newPage, cmd ) =
                    authRedirect page mode
            in
                ( { m | page = newPage, storyData = { sd | showDrawer = Nothing } }, cmd )

        ( Navigate page, _ ) ->
            let
                sd =
                    case page of
                        StoryPage _ ->
                            updateStories ClearAnswers m.storyData

                        _ ->
                            m.storyData
            in
                ( { m | storyData = sd }, Navigation.newUrl <| pageToUrl page )

        ( LoginMsg lmsg, Anon login ) ->
            let
                loginRequest username password =
                    Api.LoginRequest username password
                        |> Api.postAuthenticate

                ( loginModel, cmd, loginResponse ) =
                    Login.update loginRequest lmsg login

                ( newMode, cmds ) =
                    case loginResponse of
                        Nothing ->
                            ( Anon loginModel, [] )

                        Just lr ->
                            handleLoginResponse lr

                allCmds =
                    Cmd.batch (Cmd.map LoginMsg cmd :: cmds)
            in
                ( { m | mode = newMode }, allCmds )

        ( StoriesMsg sMsg, _ ) ->
            { m | storyData = updateStories sMsg m.storyData } ! []

        ( SchoolDataMsg sdMsg, mode ) ->
            case mode of
                TeacherMode u sd ->
                    { m | mode = TeacherMode u (updateSchoolData sdMsg sd) } ! []

                _ ->
                    -- Shouldn't happen
                    m ! []

        ( NoOp, _ ) ->
            m ! []

        -- This shouldn't be possible
        ( LoginMsg _, _ ) ->
            m ! []


updateSchoolData : SchoolDataMsg -> SchoolData -> SchoolData
updateSchoolData msg sd =
    case msg of
        ClassesResponse cs ->
            { sd | classes = cs }

        StudentsResponse ss ->
            { sd | students = ss }

        SchoolDataTableState ts ->
            { sd | tableState = ts }

        TeacherAction ta ->
            { sd | action = ta }

        StudentFormMsg formMsg ->
            { sd | addStudentsForm = AddStudentsForm.update formMsg sd.addStudentsForm }


updateStories : StoriesMsg -> StoryData -> StoryData
updateStories msg sd =
    case msg of
        StoriesResponse s ->
            { sd | stories = s }

        DictResponse d ->
            { sd | wordDict = d }

        SetTableState t ->
            { sd | tableState = t }

        StoryFilterInput f ->
            { sd | storyFilter = f }

        ToggleDrawer d ->
            if sd.showDrawer == Just d then
                { sd | showDrawer = Nothing }
            else
                { sd | showDrawer = Just d }

        ClearAnswers ->
            { sd | answersForm = AnswersForm.init }

        FormMsg formMsg ->
            { sd | answersForm = AnswersForm.update formMsg sd.answersForm }


handleLoginResponse : Api.Login -> ( AppMode, List (Cmd Msg) )
handleLoginResponse login =
    let
        token =
            AccessToken login.token

        user =
            User login.name token

        newStories =
            [ Rest.getStories token ]
    in
        case .userType login.role of
            "Teacher" ->
                ( TeacherMode user initSchoolData, Rest.getSchoolStudents token :: Rest.getSchoolClasses token :: newStories )

            "Editor" ->
                ( EditorMode user, newStories )

            "Admin" ->
                ( AdminMode user, [] )

            _ ->
                ( StudentMode user, newStories )


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.mode of
        Anon login ->
            Sub.batch [ Sub.map LoginMsg (Login.subscriptions login) ]

        _ ->
            Sub.none


view : Model -> Html Msg
view m =
    let
        panelClass =
            class "panel panel-default"

        pageContent =
            case ( m.page, m.mode ) of
                ( HomePage, _ ) ->
                    [ dashBoard m
                    , div [ id "stories", panelClass ]
                        (h2 [] [ text "Starter Stories" ] :: Stories.tilesView m.storyData)
                    ]

                ( LoginPage, Anon login ) ->
                    [ Html.map LoginMsg (Login.view login) ]

                ( FindStoryPage, _ ) ->
                    [ dashBoard m
                    , div [ id "stories", panelClass ]
                        (h2 [] [ text "Stories" ] :: Stories.tableView m.storyData)
                    ]

                ( StoryPage id_, StudentMode _ ) ->
                    [ dashBoard m
                    , Stories.viewStory m.storyData id_
                    , Stories.viewAnswersForm m.storyData
                    , drawer m.storyData.showDrawer
                    ]

                ( StoryPage id_, _ ) ->
                    [ dashBoard m
                    , Stories.viewStory m.storyData id_
                    ]

                ( TeacherPage, TeacherMode user schoolData ) ->
                    Teacher.view user schoolData

                _ ->
                    [ dashBoard m
                    , text "Haven't implemented this page yet"
                    , text (toString m)
                    ]
    in
        div []
            [ Nav.navbar (navbarLinks m)
            , div [ class "container" ]
                pageContent
            ]


navbarLinks : Model -> List (Html Msg)
navbarLinks m =
    let
        activeAttr page =
            if page == m.page then
                [ class "active" ]
            else
                []

        btn ( page, txt ) =
            li (activeAttr page)
                [ Html.a [ href (pageToUrl page) ] [ text txt ]
                ]

        showLink ( pg, _ ) =
            pageAllowed pg m.mode
    in
        List.map btn <|
            List.filter showLink
                [ ( HomePage, "Home" )
                , ( FindStoryPage, "Find a story" )
                , ( AccountPage, "My 3ML" )
                , ( LeaderBoardPage, "Leader board" )
                , ( TrailsPage, "Trails" )
                , ( TeacherPage, "Teacher" )
                , ( LoginPage, "Login" )
                , ( Logout, "Logout" )
                ]


dashBoard : Model -> Html Msg
dashBoard _ =
    div [ id "dashboard", class "panel panel-default" ]
        [ img [ src "img/robot.png" ] []
        ]


main : Program Never Model Msg
main =
    Navigation.program (locationToPage >> ChangePage)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
