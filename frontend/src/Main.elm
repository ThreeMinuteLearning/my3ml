module Main exposing (main)

import AnswersForm
import Api exposing (Story)
import Drawer exposing (drawer)
import Html exposing (Html, br, div, img, h2, h3, p, text, tr, li, label, input)
import Html.Attributes exposing (id, class, for, href, src, style, type_, value)
import Html.Events exposing (onInput)
import Login
import Markdown
import Nav
import Navigation exposing (Location)
import Regex
import RemoteData exposing (WebData)
import Routing exposing (..)
import Table
import Types exposing (..)


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
                        (h2 [] [ text "Starter Stories" ] :: storyTiles m)
                    ]

                LoginPage ->
                    [ Html.map LoginMsg (Login.view m.login) ]

                FindStoryPage ->
                    [ dashBoard m
                    , div [ id "stories", panelClass ]
                        (h2 [] [ text "Stories" ] :: viewStories m)
                    ]

                StoryPage id_ ->
                    [ dashBoard m
                    , viewStory m id_
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


mapStories : (List Story -> Html Msg) -> WebData (List Story) -> Html Msg
mapStories f stories_ =
    case stories_ of
        RemoteData.NotAsked ->
            text "Unexpected state (no stories asked for)"

        RemoteData.Loading ->
            text "Loading stories ..."

        RemoteData.Failure err ->
            text ("Error loading stories: " ++ toString err)

        RemoteData.Success s ->
            f s


storyTiles : Model -> List (Html Msg)
storyTiles m =
    let
        stories_ =
            mapStories (mkTiles << List.take 20) m.stories

        mkTiles stories =
            div [ class "storytiles" ] (List.map storyTile stories)

        storyStyle s =
            style [ ( "background", "url(pix/" ++ s.img ++ ")" ), ( "background-size", "cover" ) ]

        storyTile s =
            Html.a [ class "storytile", storyStyle s, Html.Attributes.href (pageToUrl (StoryPage (Maybe.withDefault "1" s.id))) ]
                [ h3 [] [ text s.title ] ]
    in
        [ stories_ ]


viewStories : Model -> List (Html Msg)
viewStories m =
    let
        c =
            Table.defaultCustomizations

        myThead =
            c.thead
                >> .children
                >> tr []
                >> List.singleton
                >> Table.HtmlDetails []

        tableCustomizations =
            { c | thead = myThead, tableAttrs = [ class "table table-striped" ] }

        tag i s =
            s.tags
                |> List.drop (i - 1)
                |> List.head
                |> Maybe.withDefault ""

        cfg =
            Table.customConfig
                { toId = Maybe.withDefault "" << .id
                , toMsg = SetTableState
                , columns =
                    [ storyTitleColumn
                    , Table.stringColumn "Tag1" (tag 1)
                    , Table.stringColumn "Tag2" (tag 2)
                    , Table.stringColumn "Tag3" (tag 3)
                    , levelColumn
                    ]
                , customizations = tableCustomizations
                }

        -- This is needed to make the level column wide enough so the heading and arrow
        -- don't wrap
        levelColumn : Table.Column Story Msg
        levelColumn =
            Table.veryCustomColumn
                { name = "Level"
                , viewData = \s -> Table.HtmlDetails [ style [ ( "width", "6em" ) ] ] [ Html.text (toString s.level) ]
                , sorter = Table.increasingOrDecreasingBy .level
                }

        storyTitleColumn : Table.Column Story Msg
        storyTitleColumn =
            Table.veryCustomColumn
                { name = "title"
                , viewData = viewStoryLink
                , sorter = Table.increasingOrDecreasingBy .title
                }

        viewStoryLink : Story -> Table.HtmlDetails Msg
        viewStoryLink s =
            Table.HtmlDetails []
                [ Html.a [ Html.Attributes.href (pageToUrl (StoryPage (Maybe.withDefault "1" s.id))) ] [ text s.title ]
                ]
    in
        [ div []
            [ div [ class "form-group" ]
                [ label [ for "storyfilter" ] [ text "Search" ]
                , input
                    [ type_ "text"
                    , value m.storyFilter
                    , onInput StoryFilterInput
                    , id "storyfilter"
                    ]
                    []
                , div [ class "cols-xs-4" ] []
                ]
            ]
        , div [ class "table-responsive" ]
            [ mapStories (Table.view cfg m.tableState << filterStories m.storyFilter) m.stories ]
        ]


filterStories : String -> List Story -> List Story
filterStories storyFilter stories =
    if String.length storyFilter < 3 then
        stories
    else
        let
            r =
                Regex.caseInsensitive (Regex.regex storyFilter)

            match story =
                Regex.contains r story.title || Regex.contains r story.content
        in
            List.filter match stories


viewStory : Model -> String -> Html Msg
viewStory m id_ =
    case m.stories of
        RemoteData.Success stories ->
            case List.filter (\s -> s.id == Just id_) stories of
                s :: _ ->
                    div [ class "panel panel-default" ]
                        [ h2 [] [ text s.title ]
                        , div [ id "storypic" ]
                            [ img [ src ("pix/" ++ s.img) ] []
                            ]
                        , Markdown.toHtml [ id "storycontent" ] s.content
                        , div [ id "storyfooter" ]
                            [ p [] [ text (String.join ", " s.tags), br [] [], text ("Level: " ++ toString s.level) ]
                            ]
                        ]

                _ ->
                    text "Story not found"

        _ ->
            text "Stories have not been loaded"


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
