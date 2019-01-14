module Views.WorkQueue exposing (view)

import Api
import Bootstrap exposing (closeBtn)
import Components
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Route
import Util exposing (viewUnless)
import Views.StoryTiles as StoryTiles


view : List Api.Story -> msg -> Html msg
view stories clear =
    Components.panel [ class "p-4 mb-4 relative" ]
        [ viewUnless (List.isEmpty stories) (span [ class "text-grey-dark" ] [ closeBtn clear ])
        , h1 [ class "text-lg font-light mb-1" ] [ text "My Work Queue" ]
        , if List.isEmpty stories then
            p [ class "my-2 text-grey-darker" ]
                [ text "It looks like your work queue is empty. You can add some stories to it from the "
                , a [ href (Route.routeToString Route.FindStory) ] [ text "find a story page." ]
                ]

          else
            StoryTiles.view True Nothing stories
        ]
