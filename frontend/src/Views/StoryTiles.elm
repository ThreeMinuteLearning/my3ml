module Views.StoryTiles exposing (view)

{- Displays a list of stories as a grid of tiles -}

import Api exposing (Story)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route


view : List Story -> Html msg
view stories =
    let
        storyStyle s =
            style [ ( "background", "url(pix/" ++ s.img ++ ")" ), ( "background-size", "cover" ) ]

        storyTile s =
            a [ class "storytile", storyStyle s, Html.Attributes.href (Route.routeToString (Route.Story s.id)) ]
                [ h3 [] [ text s.title ] ]
    in
        div [ class "storytiles" ] (List.map storyTile stories)
