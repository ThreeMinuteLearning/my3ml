module Views.StoryTiles exposing (view, tilesPerPage, tilesPerRow)

{- Displays a list of stories as a grid of tiles -}

import Api exposing (Story)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route


view : Bool -> List Story -> Html msg
view useSmallTiles stories =
    let
        storyStyle s =
            [ style "background" ("url(pix/" ++ s.img ++ ")" ), style "background-size" "cover"  ]

        tileClass =
            if useSmallTiles then
                "storytile smalltile"
            else
                "storytile"

        attributes s =
            [ class tileClass, Html.Attributes.href (Route.routeToString (Route.Story s.id)) ]
                ++ (storyStyle s)
                ++ if useSmallTiles then
                    [ title s.title ]
                   else
                    []

        storyTile s =
            a (attributes s)
                [ h3 [] [ text s.title ] ]
    in
        div [ id "storytiles" ] (List.map storyTile stories)


tilesPerPage : (Int, Int)-> Int
tilesPerPage (width, height) =
    Basics.min 42 ((toFloat width / 160) * (toFloat height / 145))
        |> round


tilesPerRow : Int -> Int
tilesPerRow width =
    Basics.min 6 (toFloat width / 160)
        |> round
