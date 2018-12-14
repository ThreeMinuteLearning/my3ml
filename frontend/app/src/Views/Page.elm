module Views.Page exposing (ActivePage(..), frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Bootstrap exposing (closeBtn)
import Browser exposing (Document)
import Data.Session as Session exposing (Alert(..), Session, User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)
import Svg
import Svg.Attributes as Svga
import Util
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
            [ viewHeader page session.user isLoading
            , div [ class "max-w-lg mx-auto" ]
                [ viewAlerts session.alerts onAlertClose
                , content
                ]
            ]
        ]
    }


viewHeader : ActivePage -> Maybe User -> Bool -> Html msg
viewHeader page user isLoading =
    nav [ class "flex items-center justify-between flex-wrap bg-tml-blue px-6 py-2" ]
        [ div [ class "flex items-center flex-no-shrink mr-6" ]
            [ img [ class "fill-current mr-2", src "/img/logo.png", alt "The Three Minute Learning logo (3ml)" ] []
            ]
        , input [ id "menu-toggle", type_ "checkbox", class "hidden" ] []
        , menuToggleButton
        , div [ class "menu w-full flex-grow md:flex md:items-center md:w-auto" ]
            [ div [ class "text-base md:flex-grow" ]
                (menuItems page user)
            , lazy2 Util.viewIf isLoading (div [ class "spinner bg-white" ] [])
            ]
        ]


viewAlerts : List ( Alert, Bool ) -> (Alert -> msg) -> Html msg
viewAlerts alerts onAlertClose =
    div [ id "alerts" ]
        (List.map (viewAlert onAlertClose) alerts)


viewAlert : (Alert -> msg) -> ( Alert, Bool ) -> Html msg
viewAlert onAlertClose ( a, closed ) =
    let
        ( cls, msg ) =
            case a of
                Success m ->
                    ( "alert alert-success", m )

                Error m ->
                    ( "alert alert-danger", m )

                Warning m ->
                    ( "alert alert-warning", m )

        hide =
            if closed then
                " closed"

            else
                ""
    in
    div [ class (cls ++ hide), attribute "role" "alert" ]
        [ closeBtn (onAlertClose a)
        , text msg
        ]


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


menuItems : ActivePage -> Maybe User -> List (Html msg)
menuItems page user =
    let
        home =
            navbarLink (page == Home) Route.Home [ text "Home" ]

        findStory =
            navbarLink (page == FindStory) Route.FindStory [ text "Find a story" ]

        my3ml =
            navbarLink (page == Account) Route.Account [ text "My3ml" ]

        leaderboard =
            navbarLink (page == LeaderBoard) Route.LeaderBoard [ text "Leaderboard" ]

        logout =
            navbarLink False Route.Logout [ text "Sign out" ]

        -- lazy2 Util.viewIf isLoading spinner
    in
    case user of
        Nothing ->
            [ home
            , navbarLink (page == Login) Route.Login [ text "Sign in" ]
            , navbarLink (page == Register) Route.Register [ text "Sign up" ]
            ]

        Just u ->
            case u.role of
                Session.Student ->
                    [ home, findStory, my3ml, leaderboard, logout ]

                Session.Editor ->
                    [ home, findStory, my3ml, logout ]

                Session.Teacher _ ->
                    [ home
                    , navbarLink (page == Teacher) (Route.Teacher Route.Students) [ text "Admin" ]
                    , findStory
                    , my3ml
                    , leaderboard
                    , logout
                    ]


navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    a [ classList [ ( "no-underline", not isActive ) ], class "block mt-4 md:inline-block md:mt-0 text-teal-lighter hover:text-white mr-4", classList [ ( "border-red", isActive ) ], Route.href route ] linkContent
