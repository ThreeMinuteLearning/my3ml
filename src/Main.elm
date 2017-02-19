module Main exposing (main)

import Date exposing (Date)
import Html exposing (Html, button, div, h1, p, program, text)
import Html.Attributes exposing (id, class, disabled)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode as JD
import Login exposing (User(..))
import Navigation exposing (..)
import RemoteData exposing (..)
import Routing exposing (..)
import Table


type Msg
    = ChangePage Page
    | Navigate Page
    | StoriesResponse (WebData (List Story))
    | LoginMsg Login.Msg
    | SetTableState Table.State


type alias Story =
    { id : String
    , title : String
    , content : String
    , tags : List String
    , level : Int
    , date : Date
    }


type alias Model =
    { login : Login.Model
    , user : Login.User
    , page : Page
    , stories : WebData (List Story)
    , tableState : Table.State
    }


dateDecoder : JD.Decoder Date
dateDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case Date.fromString str of
                    Err err ->
                        JD.fail err

                    Ok date ->
                        JD.succeed date
            )


storiesDecoder : JD.Decoder (List Story)
storiesDecoder =
    JD.field "stories" <|
        JD.list <|
            JD.map6 Story
                (JD.field "id" JD.string)
                (JD.field "title" JD.string)
                (JD.field "content" JD.string)
                (JD.field "tags" (JD.list JD.string))
                (JD.field "level" JD.int)
                (JD.field "date" dateDecoder)


getStories : Cmd Msg
getStories =
    Http.get "http://localhost:8000/stories.json" storiesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map StoriesResponse


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
            , stories = Loading
            , tableState = Table.initialSort "Title"
            }
    in
        ( initialModel, Cmd.batch [ cmd, Cmd.map LoginMsg loginCmd, getStories ] )


authRequired : ( Page, User ) -> Bool
authRequired pageUser =
    case pageUser of
        ( HomePage, Guest ) ->
            False

        ( LoginPage, _ ) ->
            False

        _ ->
            False


authRedirect : Page -> User -> ( Page, Cmd Msg )
authRedirect page user =
    if authRequired ( page, user ) then
        ( LoginPage, Navigation.modifyUrl <| pageToUrl LoginPage )
    else
        ( page, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        ChangePage page ->
            let
                ( newPage, cmd ) =
                    authRedirect page m.user
            in
                ( { m | page = newPage }, cmd )

        Navigate page ->
            ( m, Navigation.newUrl <| pageToUrl page )

        LoginMsg msg ->
            let
                ( loginModel, cmd, user ) =
                    Login.update msg m.login

                newUser =
                    Maybe.withDefault m.user user
            in
                ( { m | login = loginModel, user = newUser }, Cmd.map LoginMsg cmd )

        StoriesResponse s ->
            { m | stories = s } ! []

        SetTableState t ->
            { m | tableState = t } ! []


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
        pageContent =
            case m.page of
                LoginPage ->
                    Html.map LoginMsg (Login.view m.login)

                FindStoryPage ->
                    div []
                        [ dashBoard m
                        , viewStories m
                        ]

                StoryPage id ->
                    div []
                        [ dashBoard m
                        , viewStory m id
                        ]

                _ ->
                    div []
                        [ dashBoard m
                        , text "Haven't implemented this page yet"
                        , text (toString m)
                        ]
    in
        div [ id "root" ]
            [ pageContent ]


viewStories : Model -> Html Msg
viewStories m =
    let
        tag i s =
            s.tags
                |> List.drop (i - 1)
                |> List.head
                |> Maybe.withDefault ""

        cfg =
            Table.config
                { toId = .id
                , toMsg = SetTableState
                , columns =
                    [ Table.stringColumn "Title" .title
                    , Table.stringColumn "Tag1" (tag 1)
                    , Table.stringColumn "Tag2" (tag 2)
                    , Table.stringColumn "Tag3" (tag 3)
                    , Table.intColumn "Level" .level
                    , storyLinkColumn
                    ]
                }

        storyLinkColumn : Table.Column Story Msg
        storyLinkColumn =
            Table.veryCustomColumn
                { name = ""
                , viewData = viewStoryLink
                , sorter = Table.unsortable
                }

        viewStoryLink : Story -> Table.HtmlDetails Msg
        viewStoryLink s =
            Table.HtmlDetails []
                [ button [ onClick (Navigate (StoryPage s.id)) ] [ text "View" ]
                ]

        stories_ =
            case m.stories of
                NotAsked ->
                    text "Unexpected state (no stories asked for)"

                Loading ->
                    text "Loading stories ..."

                Failure err ->
                    text ("Error loading stories: " ++ toString err)

                Success stories ->
                    Table.view cfg m.tableState stories
    in
        div [ id "stories", class "section" ]
            [ h1 [] [ text "Stories" ]
            , stories_
            ]


viewStory : Model -> String -> Html Msg
viewStory m id =
    case m.stories of
        Success stories ->
            case List.filter (\s -> s.id == id) stories of
                s :: _ ->
                    div [ class "section" ]
                        [ h1 [] [ text s.title ]
                        , p [] [ text s.content ]
                        ]

                _ ->
                    text "Story not found"

        _ ->
            text "Stories have not been loaded"


dashBoard : Model -> Html Msg
dashBoard m =
    div [ id "dashboard", class "section" ]
        [ h1 [] [ text "Dashboard" ]
        , div [ id "innerdash" ]
            [ nav m
            ]
        ]


nav : Model -> Html Msg
nav m =
    let
        btn page txt =
            button
                [ onClick (Navigate page)
                , disabled (page == m.page)
                ]
                [ text txt ]
    in
        div [ id "nav" ]
            [ btn HomePage "Home"
            , btn FindStoryPage "Find a story"
            , btn AccountPage "My 3ML"
            , btn LeaderBoardPage "Leader board"
            , btn TrailsPage "Trails"
            ]


main : Program Never Model Msg
main =
    Navigation.program (locationToPage >> ChangePage)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
