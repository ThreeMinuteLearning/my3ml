module Views.Page exposing (ActivePage(..), frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Bootstrap exposing (closeBtn)
import Browser exposing (Document)
import Data.Session as Session exposing (Alert(..), Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)
import Svg
import Svg.Attributes as Svga
import Util exposing (viewUnless)
import Views.Spinner exposing (spinner)


{-| Determines which navbar link (if any) will be rendered as active.
-}
type ActivePage
    = Other
    | Home
    | Login
    | FindStory
    | Register
    | Account
    | LeaderBoard
    | Teacher


{-| Take a page's Html and frame it with a header.
-}
frame : Bool -> Session -> (Alert -> msg) -> ActivePage -> { title : String, content : Html msg } -> Document msg
frame isLoading session onAlertClose page { title, content } =
    { title = title
    , body =
        [ div [ id "app" ]
            [ viewHeader page session isLoading
            , div [ class "container mx-auto px-2" ]
                [ viewAlerts (Session.getAlerts session) onAlertClose
                , content
                ]
            ]
        ]
    }


viewHeader : ActivePage -> Session -> Bool -> Html msg
viewHeader page session isLoading =
    nav [ class "print:none bg-tml-blue mb-4" ]
        [ div [ class "container px-2 py-2 mx-auto flex items-center justify-between flex-wrap" ]
            [ div [ class "flex items-center flex-shrink-0 mr-8" ]
                [ img [ class "fill-current", src "/img/logo.png", alt "The Three Minute Learning logo (3ml)" ] []
                ]
            , input [ id "menu-toggle", type_ "checkbox", class "hidden" ] []
            , menuToggleButton
            , div [ class "menu w-full flex-grow md:flex md:items-center md:w-auto" ]
                [ div [ class "text-base md:flex-grow" ]
                    (menuItems page session)
                , lazy2 Util.viewIf isLoading (div [ class "spinner bg-white" ] [])
                ]
            ]
        ]


viewAlerts : List ( Alert, Bool ) -> (Alert -> msg) -> Html msg
viewAlerts alerts onAlertClose =
    viewUnless (List.isEmpty alerts) <|
        div [ id "alerts", class "mb-2" ]
            (List.map (viewAlert onAlertClose) alerts)


viewAlert : (Alert -> msg) -> ( Alert, Bool ) -> Html msg
viewAlert onAlertClose ( a, _ ) =
    let
        ( bAlert, txt ) =
            case a of
                Success m ->
                    ( Bootstrap.Success, m )

                Error m ->
                    ( Bootstrap.Danger, m )

                Warning m ->
                    ( Bootstrap.Danger, m )
    in
    Bootstrap.alert bAlert txt (onAlertClose a)


menuToggleButton : Html msg
menuToggleButton =
    div [ class "block md:hidden" ]
        [ label [ for "menu-toggle", class "flex items-center px-3 py-2 border rounded" ]
            [ Svg.svg [ Svga.class "fill-current text-white h-3 w-3", Svga.viewBox "0 0 20 20" ]
                [ Svg.title [] [ Svg.text "Menu" ]
                , Svg.path [ Svga.d "M0 3h20v2H0V3zm0 6h20v2H0V9zm0 6h20v2H0v-2z" ] []
                ]
            ]
        ]


menuItems : ActivePage -> Session -> List (Html msg)
menuItems page session =
    let
        home txt =
            navbarLink (page == Home) Route.Home "nav-home" txt

        findStory =
            navbarLink (page == FindStory) Route.FindStory "nav-find-story" "Find a story"

        my3ml =
            navbarLink (page == Account) Route.Account "nav-account" "My3ml"

        leaderboard =
            navbarLink (page == LeaderBoard) Route.LeaderBoard "nav-leaderboard" "Leaderboard"

        logout =
            navbarLink False Route.Logout "nav-logout" "Sign out"
    in
    if Session.isStudent session then
        [ home "My dashboard", findStory, my3ml, leaderboard, logout ]

    else if Session.isEditor session then
        [ home "Home", findStory, my3ml, logout ]

    else if Session.isTeacher session then
        [ a [ id "nav-home", href "/", target "_blank", navLinkClass ] [ text "Home" ]
        , navbarLink (page == Teacher) (Route.Teacher Route.Students) "nav-teacher-admin" "Admin"
        , findStory
        , my3ml
        , leaderboard
        , logout
        ]

    else
        [ a [ id "nav-home", href "/", navLinkClass ] [ text "Home" ]
        , navbarLink (page == Login) Route.Login "nav-login" "Sign in"
        , navbarLink (page == Register) Route.Register "nav-register" "Sign up"
        ]


navLinkClass : Attribute msg
navLinkClass =
    class "block mt-4 md:inline-block md:mt-0 text-teal-200 hover:text-white mr-4"


navbarLink : Bool -> Route -> String -> String -> Html msg
navbarLink isActive route id_ linkContent =
    a [ id id_, navLinkClass, classList [ ( "underline", isActive ), ( "border-red", isActive ) ], Route.href route ] [ text linkContent ]
