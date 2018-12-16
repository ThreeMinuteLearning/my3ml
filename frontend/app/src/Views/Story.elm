module Views.Story exposing (Msg, State, init, subscriptions, update, view)

{-| The single story view
-}

import Api exposing (Story)
import Browser.Events exposing (onResize)
import Data.Settings exposing (Settings, toStyle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as JD
import Markdown
import Ports
import Regex


type alias State =
    ( Int, Int )


type Msg
    = GetImgWidth String
    | ImageWidth Float
    | Resize Int Int


init : Int -> State
init width =
    ( 0, width )


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ Ports.imgWidth ImageWidth, onResize Resize ]


update : Msg -> State -> ( State, Cmd Msg )
update msg ( picWidth, windowWidth ) =
    case msg of
        GetImgWidth s ->
            ( ( picWidth, windowWidth ), Ports.getImgWidth s )

        ImageWidth w ->
            ( ( round w, windowWidth ), Cmd.none )

        Resize width _ ->
            ( ( picWidth, width ), Cmd.none )


view : Settings -> Story -> State -> Html Msg
view settings story ( picWidth, windowWidth ) =
    let
        ( imgDivClass, imgClass ) =
            if (toFloat picWidth / toFloat (divWidth windowWidth)) > 0.67 then
                ( "w-full", "w-full" )

            else
                ( "float-right pl-3 pb-2", "" )
    in
    div [ class "u-fade-in" ]
        [ h3 [ class "text-center text-white bg-green-dark py-2 mb-3" ] [ text story.title ]
        , div [ id "storypic", class imgDivClass ]
            [ img [ class imgClass, onLoadGetWidth GetImgWidth, src ("pix/" ++ story.img) ] []
            ]
        , Markdown.toHtml (id "storycontent" :: class "leading-normal" :: toStyle settings) (storyContent story)
        , div [ class "hidden-print text-sm" ]
            [ p [ class "mb-1" ] [ text (String.join ", " (tagList story)) ]
            , p [] [ text ("Level: " ++ String.fromInt story.level) ]
            ]
        ]


tagList : Story -> List String
tagList story =
    story.tags
        ++ Maybe.withDefault [] (Maybe.map List.singleton story.curriculum)
        ++ Maybe.withDefault [] (Maybe.map List.singleton story.qualification)


divWidth : Int -> Int
divWidth windowWidth =
    let
        padding =
            16

        responsiveWidth =
            if windowWidth >= 992 then
                992

            else if windowWidth >= 768 then
                768

            else if windowWidth >= 576 then
                576

            else
                windowWidth
    in
    responsiveWidth - 16


onLoadGetWidth : (String -> msg) -> Html.Attribute msg
onLoadGetWidth onLoad =
    Html.Events.on "load" (JD.succeed (onLoad "#storypic img"))


storyContent : Story -> String
storyContent s =
    let
        replace m =
            "*" ++ String.dropRight 1 m.match ++ "*" ++ String.right 1 m.match

        re w =
            Maybe.withDefault Regex.never <|
                Regex.fromString (w.word ++ "[^a-zA-z\\-]")

        replaceWord w content =
            Regex.replace (re w) replace content
    in
    List.foldl replaceWord s.content s.words
