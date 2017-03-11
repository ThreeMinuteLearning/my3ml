module Main exposing (main)

import AnswersForm
import Api
import Drawer exposing (drawer)
import Html exposing (Html, div, img, h2, p, text, li)
import Html.Attributes exposing (id, class, href, src)
import Login
import Nav
import Navigation exposing (Location)
import RemoteData exposing (WebData)
import Routing exposing (Page(..), locationToPage, pageToUrl)
import Stories
import Table
import Types exposing (Model, Msg(..), User(..), UserType(..))


getStories : Cmd Msg
getStories =
    Api.getStories
        |> RemoteData.sendRequest
        |> Cmd.map StoriesResponse


getDictionary : Cmd Msg
getDictionary =
    Api.getDictionary
        |> RemoteData.sendRequest
        |> Cmd.map DictResponse


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( page, cmd ) =
            authRedirect (locationToPage location) Guest

        ( loginModel, loginCmd ) =
            Login.init

        initialModel =
            { login = loginModel
            , user = Guest
            , page = page
            , stories = RemoteData.Loading
            , storyFilter = ""
            , tableState = Table.initialSort "Title"
            , showDrawer = Nothing
            , answersForm = AnswersForm.init
            , wordDict = RemoteData.Loading
            }
    in
        ( initialModel, Cmd.batch [ cmd, Cmd.map LoginMsg loginCmd, getDictionary, getStories ] )


pageAllowed : Page -> User -> Bool
pageAllowed page user =
    case user of
        Guest ->
            case page of
                HomePage ->
                    True

                LoginPage ->
                    True

                FindStoryPage ->
                    True

                StoryPage _ ->
                    True

                _ ->
                    False

        User _ userType _ ->
            case page of
                TeacherPage ->
                    userType == Teacher

                LoginPage ->
                    False

                _ ->
                    True


authRedirect : Page -> User -> ( Page, Cmd Msg )
authRedirect page user =
    if not <| pageAllowed page user then
        ( LoginPage, Navigation.modifyUrl <| pageToUrl LoginPage )
    else
        ( page, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        ChangePage Logout ->
            { m | user = Guest, stories = RemoteData.Loading, page = HomePage }
                ! [ Navigation.modifyUrl <| pageToUrl HomePage, getStories ]

        ChangePage page ->
            let
                ( newPage, cmd ) =
                    authRedirect page m.user
            in
                ( { m | page = newPage, showDrawer = Nothing }, cmd )

        Navigate page ->
            let
                answers =
                    case page of
                        StoryPage _ ->
                            AnswersForm.init

                        _ ->
                            m.answersForm
            in
                ( { m | answersForm = answers }, Navigation.newUrl <| pageToUrl page )

        LoginMsg lmsg ->
            let
                loginRequest username password =
                    Api.LoginRequest username password
                        |> Api.postAuthenticate

                ( loginModel, cmd, user ) =
                    Login.update loginRequest lmsg m.login

                newUser =
                    Maybe.withDefault m.user (Maybe.map loginResponseToUser user)
            in
                ( { m | login = loginModel, user = newUser }, Cmd.map LoginMsg cmd )

        StoriesResponse s ->
            { m | stories = s } ! []

        DictResponse d ->
            { m | wordDict = d } ! []

        SetTableState t ->
            { m | tableState = t } ! []

        StoryFilterInput f ->
            { m | storyFilter = f } ! []

        ToggleDrawer d ->
            if m.showDrawer == Just d then
                { m | showDrawer = Nothing } ! []
            else
                { m | showDrawer = Just d } ! []

        FormMsg formMsg ->
            { m | answersForm = AnswersForm.update formMsg m.answersForm } ! []


subscriptions : Model -> Sub Msg
subscriptions m =
    let
        loginSubs =
            Login.subscriptions m.login
    in
        Sub.batch
            [ Sub.map LoginMsg loginSubs
            ]


view : Model -> Html Msg
view m =
    let
        panelClass =
            class "panel panel-default"

        pageContent =
            case m.page of
                HomePage ->
                    [ dashBoard m
                    , div [ id "stories", panelClass ]
                        (h2 [] [ text "Starter Stories" ] :: Stories.tilesView m)
                    ]

                LoginPage ->
                    [ Html.map LoginMsg (Login.view m.login) ]

                FindStoryPage ->
                    [ dashBoard m
                    , div [ id "stories", panelClass ]
                        (h2 [] [ text "Stories" ] :: Stories.tableView m)
                    ]

                StoryPage id_ ->
                    [ dashBoard m
                    , Stories.viewStory m id_
                    , div [ id "activities", panelClass ]
                        [ h2 [] [ text "Answers" ]
                        , Html.map FormMsg (AnswersForm.view m.answersForm)
                        ]
                    , drawer m
                    ]

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
            pageAllowed pg m.user
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


loginResponseToUser : Api.Login -> User
loginResponseToUser login =
    let
        userType =
            case .userType login.role of
                "Teacher" ->
                    Teacher

                "Editor" ->
                    Editor

                "Admin" ->
                    Admin

                _ ->
                    Student
    in
        User login.name userType (.accessToken login.token)
