module Views.NewAccounts exposing (view)

import Api exposing (Student)
import Bootstrap exposing (closeBtn)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


view : msg -> msg -> List ( Student, ( String, String ) ) -> Html msg
view print dismiss accounts =
    let
        heading =
            h4 [ class "panel-title" ] [ text "New Accounts Created in this Session" ]

        printButton =
            a [ class "hidden-print", onClick print, href "#" ] [ text "Print this list" ]

        accountsTable =
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Username" ]
                        , th [] [ text "Password" ]
                        ]
                    ]
                , tbody [] (List.map accountRow accounts)
                ]

        accountRow ( student, ( username, password ) ) =
            tr []
                [ td [] [ text student.name ]
                , td [] [ text username ]
                , td [] [ text password ]
                ]
    in
        case accounts of
            [] ->
                div [] []

            _ ->
                div [ class "panel panel-default" ]
                    [ div [ class "panel-heading" ]
                        [ div [ class "btn-group pull-right" ]
                            [ closeBtn dismiss ]
                        , heading
                        ]
                    , div [ class "panel-body" ]
                        [ printButton
                        , accountsTable
                        ]
                    ]
