module Routing exposing (Page(..), pageToUrl, locationToPage)

import UrlParser exposing (..)
import Navigation exposing (Location)


type Page
    = HomePage
    | FindStoryPage
    | StoryPage String
    | AccountPage
    | LeaderBoardPage
    | TrailsPage
    | LoginPage
    | NotFound


pageToUrl : Page -> String
pageToUrl page =
    case page of
        HomePage ->
            "#/"

        LoginPage ->
            "#/login"

        StoryPage id ->
            "#/stories/" ++ id

        FindStoryPage ->
            "#/stories"

        AccountPage ->
            "#/account"

        LeaderBoardPage ->
            "#/leaderboard"

        TrailsPage ->
            "#/trails"

        NotFound ->
            "#notfound"


matchers : Parser (Page -> a) a
matchers =
    oneOf
        [ map HomePage top
        , map LoginPage (s "login")
        , map StoryPage (s "stories" </> string)
        , map FindStoryPage (s "stories")
        , map AccountPage (s "account")
        , map TrailsPage (s "trails")
        , map LeaderBoardPage (s "leaderboard")
        ]


locationToPage : Location -> Page
locationToPage location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFound
