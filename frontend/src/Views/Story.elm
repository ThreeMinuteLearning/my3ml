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
import Window as Window


type alias State =
    ( Int, Int )


type Msg
    = GetImgWidth String
    | ImageWidth Float
    | Resize Window.Size


init : Window.Size -> State
init size =
    ( 0, size.width )


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ Ports.imgWidth ImageWidth, Window.resizes Resize ]


update : Msg -> State -> ( State, Cmd Msg )
update msg ( picWidth, windowWidth ) =
    case msg of
        GetImgWidth s ->
            ( ( picWidth, windowWidth ), Ports.getImgWidth s )

        ImageWidth w ->
            ( round w, windowWidth ) => Cmd.none

        Resize s ->
            ( picWidth, s.width ) => Cmd.none


view : Settings -> Story -> State -> Html Msg
view settings story state =
    div [ class "u-fade-in" ]
        [ h3 [ class "storytitle" ] [ text story.title ]
        , div ((id "storypic") :: picStyle state)
            [ img
                (imgStyle state
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


thresholdWidth : Int -> Int
thresholdWidth windowWidth =
    Basics.min (round (toFloat windowWidth / 1.5)) 600


picStyle : State -> List (Html.Attribute msg)
picStyle ( picWidth, windowWidth ) =
    if picWidth > 0 && picWidth < thresholdWidth windowWidth then
        [ class "rightimage" ]
    else
        []


imgStyle : State -> List (Html.Attribute msg)
imgStyle ( picWidth, windowWidth ) =
    if picWidth > thresholdWidth windowWidth then
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
