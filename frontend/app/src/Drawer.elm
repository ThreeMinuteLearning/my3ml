module Drawer exposing (DrawerType(..), view)

import Bootstrap exposing (closeBtn)
import Html exposing (..)
import Html.Attributes exposing (checked, class, href, id, src, type_, width)
import Html.Events exposing (onCheck, onClick)


type DrawerType
    = Connect
    | Question
    | Summarise
    | Clarify


drawerToString : DrawerType -> String
drawerToString dt =
    case dt of
        Connect ->
            "Connect"

        Question ->
            "Question"

        Summarise ->
            "Summarise"

        Clarify ->
            " Clarify"


view : Maybe DrawerType -> (DrawerType -> msg) -> Html msg
view showDrawer toggleDrawer =
    let
        currentDrawer =
            Maybe.withDefault Connect showDrawer

        listItem s =
            li [ class "py-1" ] [ text s ]

        mkList is =
            List.map listItem is
                |> ul [ class "px-2 mt-4" ]

        drawerHeader =
            div [ class "pl-5 py-2 bg-white" ]
                [ h1 [ class "font-normal text-2xl text-gray-800" ]
                    [ text (drawerToString currentDrawer)
                    ]
                , span [ class "print:none text-gray-600" ]
                    [ closeBtn (toggleDrawer currentDrawer) ]
                ]

        ( content, hdrImage, panelStyle ) =
            case currentDrawer of
                Connect ->
                    ( [ mkList
                            [ "Do I know something about this already?"
                            , "Has something like this ever happened to me?"
                            , "Have I read about something like this?"
                            , "What does this remind me of in the real world?"
                            ]
                      ]
                    , "connectblack.png"
                    , "bg-teal-700"
                    )

                Summarise ->
                    ( [ p [ class "mb-2" ] [ text "We want one sentence on what this story is all about." ]
                      , p [] [ text "It doesn't have to be your own words. If there's a sentence in the story that does the job, copy and paste it. Here's what to do if there isn't:" ]
                      , mkList
                            [ "Skim the story fast, looking for good words or phrases."
                            , "Write them down."
                            , "Make a sentence by putting the words together with words of your own."
                            , "Read your sentence aloud. If it doesn't make sense, change it to make it better."
                            , "Take a last look at the story to see if you've missed any important point."
                            ]
                      ]
                    , "summariseblack.png"
                    , "bg-green-700"
                    )

                Question ->
                    ( [ p [] [ text "Here are a few questions you could ask when you're reading. Feel free to think up more of your own." ]
                      , mkList
                            [ "What does that sentence mean?"
                            , "Does this part make sense to me?"
                            , "How does the writer know that?"
                            , "Is that fact or opinion?"
                            , "How did they do that?"
                            , "Why did they do that?"
                            , "What if they had done it this way instead?"
                            , "What question is this person trying to answer?"
                            , "What happens next?"
                            ]
                      ]
                    , "questionblack.png"
                    , "bg-red-700"
                    )

                Clarify ->
                    ( [ p [] [ text "Try to figure out what the word means using these methods:" ]
                      , mkList
                            [ "Read a line or two around the word, looking for clues."
                            , "Look for parts of words or whole words in the unknown word."
                            , "Imagine the word isn't there and try another word or words in its place."
                            ]
                      ]
                    , "clarifyblack.png"
                    , "bg-pink-700"
                    )
    in
    div []
        [ input
            [ type_ "checkbox"
            , id "toggle-drawer"
            , onCheck (\_ -> toggleDrawer currentDrawer)
            , checked (showDrawer /= Nothing)
            ]
            []
        , div [ id "drawer", class panelStyle ]
            [ drawerHeader
            , div [ class "p-2 leading-normal" ] content
            ]
        ]
