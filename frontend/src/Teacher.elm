module Teacher exposing (view)

import AddClassForm
import AddStudentsForm
import Api exposing (Class, Student)
import Bootstrap exposing (toolbar, btnGroup, btn)
import Dialog
import Html exposing (Html, div, h3, p, text)
import Html.Attributes exposing (id, class, href)
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
                                    [ p [] [ text "Enter the names of up to ten students you want to add accounts for (one student per input field)." ]
                                    , AddStudentsForm.view sd.addStudentsForm
                                        |> Html.map (SchoolDataMsg << AddStudentsFormMsg)
                                    ]
                        , footer = Nothing
                        }

                AddClass ->
                    Just
                        { closeMessage = Just (SchoolDataMsg (TeacherAction ViewClasses))
                        , containerClass = Nothing
                        , header = Just (h3 [] [ text "Add Class" ])
                        , body =
                            Just <|
                                div []
                                    [ p [] [ text "Enter the new class name and a description" ]
                                    , AddClassForm.view sd.addClassForm
                                        |> Html.map (SchoolDataMsg << AddClassFormMsg)
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
    div []
        [ viewNewAccounts sd.studentAccountsCreated
        , div [ class "hidden-print" ]
            [ handleRemoteData (Table.view studentsTableConfig sd.tableState) sd.students
            ]
        ]


viewNewAccounts : List ( Student, ( String, String ) ) -> Html Msg
viewNewAccounts accounts =
    let
        heading =
            Html.h4 [] [ text "New Accounts Created in this Session" ]

        printButton =
            Html.a [ href "javascript:window.print()" ] [ text "Print this list" ]

        accountsTable =
            Html.table [ class "table" ]
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th [] [ text "Name" ]
                        , Html.th [] [ text "Username" ]
                        , Html.th [] [ text "Password" ]
                        ]
                    ]
                , Html.tbody [] (List.map accountRow accounts)
                ]

        accountRow ( student, ( username, password ) ) =
            Html.tr []
                [ Html.td [] [ text student.name ]
                , Html.td [] [ text username ]
                , Html.td [] [ text password ]
                ]

        content =
            case accounts of
                [] ->
                    []

                _ ->
                    [ heading, printButton, accountsTable ]
    in
        div [ id "newAccounts" ]
            content


viewClassesTable : SchoolData -> Html Msg
viewClassesTable sd =
    handleRemoteData (Table.view classesTableConfig sd.tableState) sd.classes


viewAnswersTable : SchoolData -> Html Msg
viewAnswersTable sd =
    div [] [ text "No answers yet..." ]
