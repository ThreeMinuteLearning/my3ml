module Teacher exposing (view)

import AddStudentsForm
import Api exposing (Class, Student)
import Bootstrap exposing (toolbar, btnGroup, btn)
import Dialog
import Html exposing (Html, div, h3, p, text)
import Html.Attributes exposing (id)
import Rest exposing (handleRemoteData)
import Table
import Types exposing (User, SchoolData, SchoolDataMsg(..), TeacherAction(..), Msg(..))


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
    let
        ( table, subtools ) =
            case sd.action of
                ViewStudents ->
                    ( viewStudentsTable sd, studentSubTools )

                ViewClasses ->
                    ( viewClassesTable sd, classesSubTools )

                AddStudents ->
                    ( viewStudentsTable sd, studentSubTools )

                AddClass ->
                    ( viewClassesTable sd, classesSubTools )

                ViewAnswers ->
                    ( viewAnswersTable sd, [] )

        studentSubTools =
            [ btn AddStudents [ text "Add Students" ] ]

        classesSubTools =
            [ btn AddClass [ text "Add Class" ] ]

        dialog =
            case sd.action of
                AddStudents ->
                    Just
                        { closeMessage = Just (SchoolDataMsg (TeacherAction ViewStudents))
                        , containerClass = Nothing
                        , header = Just (h3 [] [ text "Add Students" ])
                        , body =
                            Just <|
                                div []
                                    [ p [] [ text "Enter the names of up to ten students you want to add accounts for." ]
                                    , AddStudentsForm.view sd.addStudentsForm
                                        |> Html.map (SchoolDataMsg << StudentFormMsg)
                                    ]
                        , footer = Nothing
                        }

                _ ->
                    Nothing
    in
        [ Html.map (SchoolDataMsg << TeacherAction) <|
            toolbar "toolbar"
                [ btnGroup
                    [ btn ViewStudents [ text "Students" ]
                    , btn ViewClasses [ text "Classes" ]
                    , btn ViewAnswers [ text "Answers" ]
                    ]
                , btnGroup subtools
                ]
        , div [ id "students" ]
            [ table
            , Dialog.view dialog
            ]
        ]


viewStudentsTable : SchoolData -> Html Msg
viewStudentsTable sd =
    handleRemoteData (Table.view studentsTableConfig sd.tableState) sd.students


viewClassesTable : SchoolData -> Html Msg
viewClassesTable sd =
    handleRemoteData (Table.view classesTableConfig sd.tableState) sd.classes


viewAnswersTable : SchoolData -> Html Msg
viewAnswersTable sd =
    div [] [ text "No answers yet..." ]
