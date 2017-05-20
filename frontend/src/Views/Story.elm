module Views.Story exposing (view)

{-| The single story view
-}

import Api exposing (Story)
import Html exposing (Html, br, div, h2, img, p, text)
import Html.Attributes exposing (id, class, for, src, style, type_, value)
import Html.Events
import Json.Decode as JD
import Markdown
import Regex


view : Story -> Int -> (String -> msg) -> Html msg
view story picWidth onPicLoad =
    div []
        [ h2 [] [ text story.title ]
        , div [ id "storypic", picStyle picWidth ]
            [ img [ onLoadGetWidth onPicLoad, src ("pix/" ++ story.img) ] []
            ]
        , Markdown.toHtml [ id "storycontent" ] (storyContent story)
        , div [ id "storyfooter" ]
            [ p [] [ text (String.join ", " story.tags), br [] [], text ("Level: " ++ toString story.level) ]
            ]
        ]


picStyle : Int -> Html.Attribute msg
picStyle width =
    if width > 0 && width < 300 then
        style [ ( "float", "right" ) ]
    else
        style []


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
