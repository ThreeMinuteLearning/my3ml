module Views.Story exposing (State, Msg, init, view, subscriptions, update)

{-| The single story view
-}

import Api exposing (Story)
import Data.Settings exposing (Settings, toStyle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as JD
import Markdown
import Ports
import Regex
import Util exposing ((=>))


type alias State =
    Int


type Msg
    = GetImgWidth String
    | ImageWidth Float


init : State
init =
    0


subscriptions : Sub Msg
subscriptions =
    Ports.imgWidth ImageWidth


update : Msg -> State -> ( State, Cmd Msg )
update msg picWidth =
    case msg of
        GetImgWidth s ->
            ( picWidth, Ports.getImgWidth s )

        ImageWidth w ->
            round w => Cmd.none


view : Settings -> Story -> State -> Html Msg
view settings story picWidth =
    div [ class "u-fade-in" ]
        [ h3 [ class "storytitle" ] [ text story.title ]
        , div ((id "storypic") :: picStyle picWidth)
            [ img
                (imgStyle picWidth
                    ++ [ onLoadGetWidth GetImgWidth, src ("pix/" ++ story.img) ]
                )
                []
            ]
        , Markdown.toHtml [ id "storycontent", toStyle settings ] (storyContent story)
        , div [ id "storyfooter", class "hidden-print" ]
            [ p [] [ text (String.join ", " (tagList story)), br [] [], text ("Level: " ++ toString story.level) ]
            ]
        ]


tagList : Story -> List String
tagList story =
    story.tags ++ Maybe.withDefault [] (Maybe.map List.singleton story.curriculum) ++ [ story.qualification ]


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
