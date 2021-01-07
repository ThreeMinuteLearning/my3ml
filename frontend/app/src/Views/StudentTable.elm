module Views.StudentTable exposing (config, init, view)

import Api
import Bootstrap exposing (link)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Route
import Table
import Time exposing (Month(..))
import Tuple exposing (second)


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

        createdAtColumn =
            Table.customColumn
                { name = "Created On"
                , viewData = \( _, student ) -> posixToDate ( Time.millisToPosix (round (student.createdAt * 1000) ))
                , sorter = Table.increasingOrDecreasingBy (\(_, s) -> s.createdAt)
                }
    in
    Table.customConfig
        { toId = .id << second
        , toMsg = setState
        , columns =
            [ checkboxColumn
            , nameColumn
            , Table.intColumn "Level" (.level << second)
            , createdAtColumn
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

posixToDate : Time.Posix -> String
posixToDate t =
    let
        year = String.fromInt (Time.toYear Time.utc t)
        month = case Time.toMonth Time.utc t of
            Jan -> "01"
            Feb -> "02"
            Mar -> "03"
            Apr -> "04"
            May -> "05"
            Jun -> "06"
            Jul -> "07"
            Aug -> "08"
            Sep -> "09"
            Oct -> "10"
            Nov -> "11"
            Dec -> "12"
        day = String.fromInt (Time.toDay Time.utc t)
    in
    year ++ "-" ++ month ++ "-" ++ day
