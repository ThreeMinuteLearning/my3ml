module Route exposing (Route(..), TeacherSubRoute(..), href, modifyUrl, fromLocation, routeToString)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing (parseHash, s, (</>), string, oneOf, Parser)


-- ROUTING --


type Route
    = Home
    | Login
    | Register
    | Logout
    | Account
    | FindStory
    | Story String
    | LeaderBoard
    | Trails
    | Teacher TeacherSubRoute


type TeacherSubRoute
    = Students
    | Classes
    | Student String


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Register (s "register")
        , Url.map Logout (s "logout")
        , Url.map Account (s "account")
        , Url.map Story (s "stories" </> string)
        , Url.map FindStory (s "stories")
        , Url.map Trails (s "trails")
        , Url.map Teacher (s "teacher" </> teacherSubRoute)
        , Url.map LeaderBoard (s "leaderboard")
        ]


teacherSubRoute : Parser (TeacherSubRoute -> a) a
teacherSubRoute =
    oneOf
        [ Url.map Student (s "students" </> string)
        , Url.map Students (s "students")
        , Url.map Classes (s "classes")
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Register ->
                    [ "register" ]

                Logout ->
                    [ "logout" ]

                Account ->
                    [ "account" ]

                Story slug ->
                    [ "stories", slug ]

                FindStory ->
                    [ "stories" ]

                LeaderBoard ->
                    [ "leaderboard" ]

                Teacher Students ->
                    [ "teacher", "students" ]

                Teacher Classes ->
                    [ "teacher", "classes" ]

                Teacher (Student slug) ->
                    [ "teacher", "students", slug ]

                Trails ->
                    [ "trails" ]
    in
        "#/" ++ (String.join "/" pieces)



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
