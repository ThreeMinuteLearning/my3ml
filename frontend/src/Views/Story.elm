module Views.Story exposing (view)

{-| The single story view
-}

import Api exposing (Story)
import Data.Settings exposing (Settings, toStyle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as JD
import Markdown
import Regex


view : Settings -> Story -> Int -> (String -> msg) -> Html msg
view settings story picWidth onPicLoad =
    div []
        [ h3 [ class "storytitle" ] [ text story.title ]
        , div ((id "storypic") :: picStyle picWidth)
            [ img
                (imgStyle picWidth
                    ++ [ onLoadGetWidth onPicLoad, src ("pix/" ++ story.img) ]
                )
                []
            ]
        , Markdown.toHtml [ id "storycontent", toStyle settings ] (storyContent story)
        , div [ id "storyfooter" ]
            [ p [] [ text (String.join ", " story.tags), br [] [], text ("Level: " ++ toString story.level) ]
            ]
        ]


picStyle : Int -> List (Html.Attribute msg)
picStyle width =
    if width > 0 && width < 600 then
        [ class "rightimage" ]
    else
        []


imgStyle : Int -> List (Html.Attribute msg)
imgStyle width =
    if width > 600 then
        [ style [ ( "width", "100%" ) ] ]
    else
        []


onLoadGetWidth : (String -> msg) -> Html.Attribute msg
onLoadGetWidth onLoad =
    Html.Events.on "load" (JD.succeed (onLoad "#storypic img"))


storyContent : Story -> String
storyContent s =
    let
        replace m =
            "*" ++ (String.dropRight 1 m.match) ++ "*" ++ (String.right 1 m.match)

        re w =
            Regex.regex ((Regex.escape w.word) ++ "[^a-zA-z\\-]")

        replaceWord w content =
            Regex.replace Regex.All (re w) replace content
    in
        List.foldl replaceWord s.content s.words
