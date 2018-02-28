module Views.TeacherToolbar exposing (view)

import Bootstrap exposing (toolbar, btnGroup)
import Data.Session exposing (Session, isSchoolAdmin)
import Exts.Html.Bootstrap exposing (formGroup, row)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route
import Util exposing (viewIf)


view : Session -> List (Html msg) -> Html msg
view session subtools =
    row
        [ toolbar "toolbar"
            [ btnGroup
                [ btn "students-button" (routeTo Route.Students) [ text "Students" ]
                , btn "classes-button" (routeTo Route.Classes) [ text "Classes" ]
                , viewIf (isSchoolAdmin session)
                    (btn "teachers-button" (routeTo Route.Teachers) [ text "Teachers" ])
                ]
            , btnGroup subtools
            ]
        ]


btn : String -> String -> List (Html msg) -> Html msg
btn id_ link =
    a [ id id_, class "btn btn-default", href link ]


routeTo : Route.TeacherSubRoute -> String
routeTo r =
    Route.routeToString (Route.Teacher r)
