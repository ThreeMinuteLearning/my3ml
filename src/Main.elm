module Main exposing (main)

import Html exposing (Html, button, div, h1, program, text)
import Html.Attributes exposing (id, class, disabled)
import Html.Events exposing (onClick)
import Login exposing (User(..))
import Navigation exposing (..)
import Routing exposing (..)


type Msg
    = ChangePage Page
    | Navigate Page
    | LoginMsg Login.Msg


type alias Model =
    { login : Login.Model
    , user : Login.User
    , page : Page
    }


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
            }
    in
        ( initialModel, Cmd.batch [ cmd, Cmd.map LoginMsg loginCmd ] )


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

                _ ->
                    div []
                        [ dashBoard m
                        , text "Haven't implemented this page yet"
                        , text (toString m)
                        ]
    in
        div [ id "root" ]
            [ pageContent ]


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
