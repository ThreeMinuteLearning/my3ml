module Views.StoryTiles exposing (divId, tilesPerPage, tilesPerRow, view)

{- Displays a list of stories as a grid of tiles -}

import Api exposing (Story)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Route
import Tailwinds exposing (breakpoints)


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

        tileClass =
            if useSmallTiles then
                "w-20 h-16 mb-2 px-1"

            else
                "w-1/2 sm:w-1/3 md:w-1/4 mb-2 h-32 lg:h-40 px-1"

        styleAttrs s =
            [ class "block shadow w-full h-full p-1"
            , style "background-image" ("url(/thumbs/" ++ s.img ++ ")")
            , style "background-size" "cover"
            , style "background-position" "center"
            , style "box-shadow" "1px 1px 3px rgba(0,0,0,0.3)"
            , title s.title
            ]

        storyTile s =
            div [ class tileClass ]
                [ a (actionAttrs s ++ styleAttrs s)
                    [ h3 [ class "text-sm text-bold text-white", classList [ ( "hidden", useSmallTiles ) ] ] [ text s.title ]
                    ]
                ]
    in
    div [ id divId, class "flex flex-wrap -mx-1" ] (List.map storyTile stories)


tilesPerPage : ( Int, Int ) -> Int
tilesPerPage ( width, height ) =
    let
        -- Tailwinds h-32 is 8rem, h-40 is 10rem, mb-2 is 0.5 rem, assume standard base of 16px
        tileHeightIncMarginPx =
            if width >= breakpoints.lg then
                16 * 8 + 8

            else
                16 * 10 + 8
    in
    tilesPerRow width * (height // tileHeightIncMarginPx + 1)


tilesPerRow : Int -> Int
tilesPerRow width =
    if width >= breakpoints.lg then
        5

    else if width >= breakpoints.md then
        4

    else if width >= breakpoints.sm then
        3

    else
        2
