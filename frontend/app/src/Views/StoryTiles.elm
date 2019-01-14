module Views.StoryTiles exposing (divId, tilesPerPage, tilesPerRow, view)

{- Displays a list of stories as a grid of tiles -}

import Api exposing (Story)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Route


divId : String
divId =
    "storytiles"


view : Bool -> Maybe (Int -> msg) -> List Story -> Html msg
view useSmallTiles action stories =
    let
        actionAttrs s =
            case action of
                Nothing ->
                    [ href (Route.routeToString (Route.Story s.id)) ]

                Just doThis ->
                    [ onClick (doThis s.id), href "" ]

        styleAttrs s =
            [ class "inline-block flex-auto no-underline shadow px-2 py-1 mr-2 mb-2"
            , style "width" "9rem"
            , style "height" "7.5rem"
            , style "background" ("url(pix/" ++ s.img ++ ")")
            , style "background-size" "cover"
            , style "box-shadow" "1px 1px 3px rgba(0,0,0,0.3)"
            , title s.title
            ]

        storyTile s =
            a (actionAttrs s ++ styleAttrs s)
                [ h3 [ class "text-sm text-white", classList [ ( "hidden", useSmallTiles ) ] ] [ text s.title ]
                ]
    in
    div [ id divId, class "flex flex-wrap justify-between" ] (List.map storyTile stories)


tilesPerPage : ( Int, Int ) -> Int
tilesPerPage ( width, height ) =
    Basics.max 25 ((toFloat width / 152) * (toFloat height / 128))
        |> round


tilesPerRow : Int -> Int
tilesPerRow width =
    Basics.min 5 (toFloat width / 152)
        |> round
