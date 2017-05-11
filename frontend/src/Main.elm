module Main exposing (main)

import AddClassForm
import AddStudentsForm
import AnswersForm
import Api
import Dict
import Form
import Html exposing (Html, div, img, h2, text, li)
import Html.Attributes exposing (id, class, href, src)
import Login
import Nav
import Navigation exposing (Location)
import Ports
import RemoteData
import Rest
import Routing exposing (Page(..), locationToPage, pageToUrl)
import Stories
import Table
import Teacher
import Tuple exposing (first, second)
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
    , currentPicWidth = 0
    , currentStory = Nothing
    , tableState = Table.initialSort "Title"
    , answersForm = Nothing
    , myAnswers = RemoteData.NotAsked
    , wordDict = RemoteData.Loading
    }


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

        ( LeaderBoardPage, StudentMode _ ) ->
            True

        ( LeaderBoardPage, _ ) ->
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

                newStoryData s =
                    Just ( { sd | answersForm = Just (AnswersForm.init s) }, Cmd.batch [ cmd, Ports.postProcessStory s.words ] )

                ( newSd, newCmd ) =
                    case newPage of
                        StoryPage id_ ->
                            -- TODO This is duplicated work here
                            -- since the current story is also found in the view
                            -- It should probably be cached in the model
                            Stories.findById sd id_
                                |> Maybe.andThen newStoryData
                                |> Maybe.withDefault ( sd, cmd )

                        _ ->
                            ( sd, cmd )
            in
                ( { m | page = newPage, storyData = newSd }, newCmd )

        ( Navigate page, _ ) ->
            ( m, Navigation.newUrl <| pageToUrl page )

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

        ( StoriesMsg sMsg, mode ) ->
            let
                ( newSd, cmd ) =
                    updateStories (userFromMode mode) sMsg m.storyData
            in
                { m | storyData = newSd } ! [ cmd ]

        ( SchoolDataMsg sdMsg, mode ) ->
            case mode of
                TeacherMode u sd ->
                    let
                        ( newSd, cmd ) =
                            updateSchoolData u sdMsg sd
                    in
                        { m | mode = TeacherMode u newSd } ! [ cmd ]

                _ ->
                    -- Shouldn't happen
                    m ! []

        ( GetImgWidth s, _ ) ->
            m ! [ Ports.getImgWidth s ]

        ( ImageWidth w, _ ) ->
            let
                sd =
                    m.storyData
            in
                { m | storyData = { sd | currentPicWidth = round w } } ! []

        ( DictLookup w, _ ) ->
            m ! []

        ( PrintWindow, _ ) ->
            m ! [ Ports.printWindow () ]

        ( NoOp, _ ) ->
            m ! []

        -- This shouldn't be possible
        ( LoginMsg _, _ ) ->
            m ! []


userFromMode : AppMode -> User
userFromMode mode =
    case mode of
        TeacherMode u _ ->
            u

        StudentMode u ->
            u

        _ ->
            User "Guest" (AccessToken "")


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


formCompleted : Form.Msg -> Form.Form e output -> Maybe output
formCompleted msg form =
    case ( msg, Form.getOutput form ) of
        ( Form.Submit, Just o ) ->
            Just o

        _ ->
            Nothing


updateStories : User -> StoriesMsg -> StoryData -> ( StoryData, Cmd Msg )
updateStories (User _ token) msg sd =
    case msg of
        StoriesResponse s ->
            { sd | stories = s } ! []

        DictResponse d ->
            { sd | wordDict = d } ! []

        SetTableState t ->
            { sd | tableState = t } ! []

        StoryFilterInput f ->
            { sd | storyFilter = f } ! []

        ClearAnswers ->
            resetAnswersForm sd ! []

        AnswersFormMsg answerFormMsg ->
            case ( sd.answersForm, answerFormMsg ) of
                ( Just m, AnswersForm.FormMsg formMsg ) ->
                    case formCompleted formMsg (.form m) of
                        Just answers ->
                            sd ! [ Rest.submitAnswers token (.story m) answers ]

                        _ ->
                            { sd | answersForm = Maybe.map (AnswersForm.update answerFormMsg) sd.answersForm } ! []

                ( _, _ ) ->
                    { sd | answersForm = Maybe.map (AnswersForm.update answerFormMsg) sd.answersForm } ! []

        GetAnswersResponse r ->
            { sd | myAnswers = r } ! []

        SubmitAnswersResponse r ->
            case r of
                RemoteData.Success answer ->
                    -- TODO show answers, not the form
                    let
                        newAnswers =
                            RemoteData.map ((::) answer) sd.myAnswers

                        newSD =
                            resetAnswersForm sd
                    in
                        { newSD | myAnswers = newAnswers } ! []

                -- TODO Post message on failure
                _ ->
                    sd ! []


resetAnswersForm : StoryData -> StoryData
resetAnswersForm sd =
    case sd.answersForm of
        Nothing ->
            sd

        Just m ->
            { sd | answersForm = Just (AnswersForm.init (.story m)) }


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
                ( StudentMode user, (Rest.getAnswers token Nothing) :: newStories )


subscriptions : Model -> Sub Msg
subscriptions m =
    let
        loginSub =
            case m.mode of
                Anon login ->
                    Sub.map LoginMsg (Login.subscriptions login)

                _ ->
                    Sub.none

        storyPageSubs =
            case m.page of
                StoryPage _ ->
                    Sub.batch [ Ports.imgWidth ImageWidth, Ports.dictLookup DictLookup ]

                _ ->
                    Sub.none
    in
        Sub.batch [ storyPageSubs, loginSub ]


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
