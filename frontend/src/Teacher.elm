module Teacher exposing (view)

import Api exposing (Class, Student)
import Bootstrap exposing (toolbar, btnGroup, btn)
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Rest exposing (handleRemoteData)
import Table
import Types exposing (User, SchoolData, SchoolDataMsg(..), Msg(..))


classesTableConfig : Table.Config Class Msg
classesTableConfig =
    Table.customConfig
        { toId = .id
        , toMsg = SchoolDataMsg << SchoolDataTableState
        , columns =
            [ Table.stringColumn "Class Name" .name
            , Table.intColumn "Number of Students" (List.length << .students)
            ]
        , customizations = Bootstrap.tableCustomizations
        }


studentsTableConfig : Table.Config Student Msg
studentsTableConfig =
    Table.customConfig
        { toId = .id
        , toMsg = SchoolDataMsg << SchoolDataTableState
        , columns =
            [ Table.stringColumn "Name" .name
            , Table.intColumn "Level" .level
            ]
        , customizations = Bootstrap.tableCustomizations
        }


view : User -> SchoolData -> List (Html Msg)
view _ sd =
    [ toolbar "toolbar"
        [ btnGroup
            [ btn NoOp [ text "Classes" ]
            , btn NoOp [ text "Students" ]
            , btn NoOp [ text "Answers" ]
            , btn NoOp [ text "Add Students" ]
            , btn NoOp [ text "Do Something" ]
            ]
        ]
    , div [ id "students" ]
        [ handleRemoteData (Table.view studentsTableConfig sd.tableState) sd.students
        ]
    ]
