module Views.StudentTable exposing (config, init, view)

import Api
import Bootstrap exposing (link)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Route
import Table
import Tuple exposing (first, second)


init : Table.State
init =
    Table.initialSort "Name"


view : Table.Config ( Bool, Api.Student ) msg -> Table.State -> List Api.Student -> (Api.Student -> Bool) -> Html msg
view cfg state students isChecked =
    Table.view cfg state (List.map (\s -> ( isChecked s, s )) students)


config : (Table.State -> msg) -> (Api.Student -> Bool -> msg) -> Table.Config ( Bool, Api.Student ) msg
config setState onSelectStudent =
    let
        checkboxColumn =
            Table.veryCustomColumn
                { name = ""
                , viewData = viewCheckbox
                , sorter = Table.unsortable
                }

        viewCheckbox ( selected, s ) =
            Table.HtmlDetails []
                [ input [ type_ "checkbox", onCheck (onSelectStudent s), checked selected ] []
                ]

        nameColumn =
            Table.veryCustomColumn
                { name = "Name"
                , viewData = viewStudentLink
                , sorter = Table.increasingOrDecreasingBy (second >> .name)
                }

        viewStudentLink ( _, student ) =
            Table.HtmlDetails []
                [ link (Route.href (Route.Teacher (Route.Student student.id))) student.name
                ]
    in
    Table.customConfig
        { toId = .id << second
        , toMsg = setState
        , columns =
            [ checkboxColumn
            , nameColumn
            , Table.intColumn "Level" (.level << second)
            , Table.stringColumn "Hidden"
                (\( _, s ) ->
                    if s.hidden then
                        "x"

                    else
                        ""
                )
            , Table.stringColumn "Deleted"
                (\( _, s ) ->
                    case s.deleted of
                        Nothing ->
                            ""

                        _ ->
                            "x"
                )
            ]
        , customizations = Bootstrap.tableCustomizations
        }
