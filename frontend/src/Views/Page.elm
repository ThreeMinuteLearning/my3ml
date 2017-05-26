module Views.Page exposing (frame, ActivePage(..))

{-| The frame around a typical page - that is, the header and footer.
-}

import Data.Session as Session exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)
import Util exposing ((=>))
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


{-| Take a page's Html and frame it with a header and footer.
-}
frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
frame isLoading user page content =
    div [ class "page-frame" ]
        [ viewHeader page user isLoading
        , content
        , viewFooter
        ]


viewHeader : ActivePage -> Maybe User -> Bool -> Html msg
viewHeader page user isLoading =
    header []
        [ nav [ class "navbar navbar-default" ]
            [ div [ class "container" ]
                [ div [ class "navbar-header" ]
                    [ mobileToggleButton
                    , a [ class "navbar-brand", Route.href Route.Home ] [ text "3ML" ]
                    ]
                , div [ id "navbar", class "navbar-collapse collapse" ]
                    [ ul [ class "nav navbar-nav navbar-right" ] <|
                        lazy2 Util.viewIf isLoading spinner
                            :: (navbarLink (page == Home) Route.Home [ text "Home" ])
                            :: viewSignIn page user
                    ]
                ]
            ]
        ]


mobileToggleButton : Html msg
mobileToggleButton =
    button [ type_ "button", class "navbar-toggle collapsed", attribute "data-toggle" "collapse", attribute "data-target" "#navbar", attribute "aria-expanded" "false" ]
        [ span [ class "sr-only" ] [ text "Toggle Navigation" ]
        , span [ class "icon-bar" ] []
        , span [ class "icon-bar" ] []
        , span [ class "icon-bar" ] []
        ]


viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
    case user of
        Nothing ->
            [ navbarLink (page == Login) Route.Login [ text "Sign in" ]
            , navbarLink (page == Register) Route.Register [ text "Sign up" ]
            ]

        Just user ->
            case user.role of
                Session.Student ->
                    standardNavLinks page

                Session.Teacher ->
                    (navbarLink (page == Teacher) (Route.Teacher Route.Students) [ text "Teacher" ])
                        :: standardNavLinks page


standardNavLinks : ActivePage -> List (Html msg)
standardNavLinks page =
    [ navbarLink (page == FindStory) Route.FindStory [ text " Find a story" ]
    , navbarLink (page == Account) Route.Account [ text " My3ML" ]
    , navbarLink False Route.Logout [ text "Sign out" ]
    ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "3ML " ]
            , span [ class "attribution" ]
                [ text "An interactive learning project from Three Minute Learning Ltd."
                ]
            ]
        ]


navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]



{-
   bodyId : String
   bodyId =
       "page-body"
-}
