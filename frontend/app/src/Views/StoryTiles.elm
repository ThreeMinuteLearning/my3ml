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

        ( flex, flexItem, ( width, height ) ) =
            if useSmallTiles then
                ( "flex flex-wrap", "flex-initial", ( "4.5rem", "3.75rem" ) )

            else
                ( "flex flex-wrap flex-between", "flex-auto lg:flex-initial", ( "9rem", "7.5rem" ) )

        styleAttrs s =
            [ class "inline-block no-underline shadow px-2 py-1 mr-2 mb-2"
            , class flexItem
            , style "width" width
            , style "height" height
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
    div [ id divId, class flex ] (List.map storyTile stories)


tilesPerPage : ( Int, Int ) -> Int
tilesPerPage ( width, height ) =
    Basics.max 25 ((toFloat width / 152) * (toFloat height / 128))
        |> round


tilesPerRow : Int -> Int
tilesPerRow width =
    Basics.min 5 (toFloat width / 152)
        |> round
