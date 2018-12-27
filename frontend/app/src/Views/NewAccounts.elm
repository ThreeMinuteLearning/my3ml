module Views.NewAccounts exposing (view)

import Api exposing (Student)
import Bootstrap exposing (closeBtn)
import Components
import Html exposing (..)
import Html.Attributes exposing (..)
import Util exposing (printButton)


view : msg -> msg -> List ( Student, ( String, String ) ) -> Html msg
view print dismiss accounts =
    let
        accountsTable =
            table [ class "w-full" ]
                [ thead []
                    [ tr []
                        [ th [ class "text-left" ] [ text "Name" ]
                        , th [ class "text-left" ] [ text "Username" ]
                        , th [ class "text-left" ] [ text "Password" ]
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
            text ""

        _ ->
            Components.panel [ class "p-4 mb-4 relative" ]
                [ span [ class "print:none text-grey-dark" ] [ closeBtn dismiss ]
                , h1 [ class "print:none text-xl font-light mb-2" ] [ text "New Accounts Created in this Session" ]
                , div [ class "p-2" ]
                    [ printButton print "Print list of new accounts"
                    , p [ class "print:none my-2" ] [ text "Please make sure you save this list or print it off before logging out, or the information will be lost." ]
                    , p [ class "print:none mb-2" ] [ text "Passwords and usernames are automatically generated. You can change them later. Please don't use real names or other personal information for usernames." ]
                    , accountsTable
                    ]
                ]
