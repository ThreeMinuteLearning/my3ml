module Views.TeacherToolbar exposing (view)

import Data.Session exposing (Session, isSchoolAdmin)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route
import Util exposing (viewIf)


view : Session -> Route.TeacherSubRoute -> List (Html msg) -> Html msg
view session page subtools =
    div [ id "teacher-toolbar", class "flex justify-between flex-wrap" ]
        [ ul [ class "list-reset flex-1 flex border-b mr-4" ]
            [ btn "students-button" page Route.Students "Students"
            , btn "classes-button" page Route.Classes "Classes"
            , viewIf (isSchoolAdmin session)
                (btn "teachers-button" page Route.Teachers "Teachers")
            ]
        , div [ class "flex" ]
            subtools
        ]


btn : String -> Route.TeacherSubRoute -> Route.TeacherSubRoute -> String -> Html msg
btn id_ page link txt =
    if page == link then
        li [ class "-mb-px mr-1" ]
            [ a [ id id_, class "bg-white inline-block border-l border-t border-r rounded-t py-2 px-4 text-blue-dark font-semibold", href (routeTo link) ] [ text txt ]
            ]

    else
        li [ class "mr-1" ]
            [ a [ id id_, class "bg-white inline-block py-2 px-4 text-blue hover:text-blue-dark font-semibold", href (routeTo link) ] [ text txt ]
            ]


routeTo : Route.TeacherSubRoute -> String
routeTo r =
    Route.routeToString (Route.Teacher r)
