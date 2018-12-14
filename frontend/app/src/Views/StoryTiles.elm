module Views.StoryTiles exposing (tilesPerPage, tilesPerRow, view)

{- Displays a list of stories as a grid of tiles -}

import Api exposing (Story)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route


view : Bool -> List Story -> Html msg
view useSmallTiles stories =
    let
        storyTile s =
            a
                [ class "inline-block no-underline shadow px-2 py-1 mr-2 mt-2"
                , style "width" "9rem"
                , style "height" "7.5rem"
                , style "background" ("url(pix/" ++ s.img ++ ")")
                , style "background-size" "cover"
                , style "box-shadow" "1px 1px 3px rgba(0,0,0,0.3)"
                , href (Route.routeToString (Route.Story s.id))
                , title s.title
                ]
                [ h3 [ class "text-sm text-white", classList [ ( "hidden", useSmallTiles ) ] ] [ text s.title ]
                ]
    in
    div [ class "flex flex-wrap" ] (List.map storyTile stories)


tilesPerPage : ( Int, Int ) -> Int
tilesPerPage ( width, height ) =
    Basics.max 25 ((toFloat width / 152) * (toFloat height / 128))
        |> round


tilesPerRow : Int -> Int
tilesPerRow width =
    Basics.min 5 (toFloat width / 152)
        |> round
