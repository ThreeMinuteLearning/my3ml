module Route exposing (Route(..), TeacherSubRoute(..), href, modifyUrl, fromUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, int, string)


-- ROUTING --


type Route
    = Home
    | Login
    | Register
    | Logout
    | Account
    | FindStory
    | Story Int
    | LeaderBoard
    | Trails
    | Teacher TeacherSubRoute
    | Editor Int


type TeacherSubRoute
    = Students
    | Classes
    | Teachers
    | Student String
    | Class String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Register (s "register")
        , Parser.map Logout (s "logout")
        , Parser.map Account (s "account")
        , Parser.map Story (s "stories" </> int)
        , Parser.map FindStory (s "stories")
        , Parser.map Trails (s "trails")
        , Parser.map Teacher (s "teacher" </> teacherSubRoute)
        , Parser.map LeaderBoard (s "leaderboard")
        , Parser.map Editor (s "editor" </> int)
        ]


teacherSubRoute : Parser (TeacherSubRoute -> a) a
teacherSubRoute =
    oneOf
        [ Parser.map Student (s "students" </> string)
        , Parser.map Students (s "students")
        , Parser.map Class (s "classes" </> string)
        , Parser.map Classes (s "classes")
        , Parser.map Teachers (s "teachers")
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
                    [ "stories", String.fromInt slug ]

                FindStory ->
                    [ "stories" ]

                LeaderBoard ->
                    [ "leaderboard" ]

                Teacher Students ->
                    [ "teacher", "students" ]

                Teacher Classes ->
                    [ "teacher", "classes" ]

                Teacher Teachers ->
                    [ "teacher", "teachers" ]

                Teacher (Student slug) ->
                    [ "teacher", "students", slug ]

                Teacher (Class slug) ->
                    [ "teacher", "classes", slug ]

                Trails ->
                    [ "trails" ]

                Editor slug ->
                    [ "editor", String.fromInt slug ]
    in
        "#/" ++ (String.join "/" pieces)



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Nav.Key -> Route -> Cmd msg
modifyUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
