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
                [ btn (routeTo Route.Students) [ text "Students" ]
                , btn (routeTo Route.Classes) [ text "Classes" ]
                , viewIf (isSchoolAdmin session)
                    (btn (routeTo Route.Teachers) [ text "Teachers" ])
                ]
            , btnGroup subtools
            ]
        ]


btn : String -> List (Html msg) -> Html msg
btn link =
    a [ class "btn btn-default", href link ]


routeTo : Route.TeacherSubRoute -> String
routeTo r =
    Route.routeToString (Route.Teacher r)
