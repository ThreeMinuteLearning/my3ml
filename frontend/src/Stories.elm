module Stories exposing (tableView, tilesView, viewStory)

import Api exposing (Story)
import Html exposing (Html, br, div, img, h2, h3, p, text, tr, li, label, input)
import Html.Attributes exposing (id, class, for, href, src, style, type_, value)
import Html.Events exposing (onInput)
import Markdown
import Regex
import RemoteData exposing (WebData)
import Routing exposing (pageToUrl, Page(..))
import Table
import Types exposing (Model, Msg(..))


mapStories : (List Story -> Html Msg) -> WebData (List Story) -> Html Msg
mapStories f stories_ =
    case stories_ of
        RemoteData.NotAsked ->
            text "Unexpected state (no stories asked for)"

        RemoteData.Loading ->
            text "Loading stories ..."

        RemoteData.Failure err ->
            text ("Error loading stories: " ++ toString err)

        RemoteData.Success s ->
            f s


tilesView : Model -> List (Html Msg)
tilesView m =
    let
        stories_ =
            mapStories (mkTiles << List.take 20) m.stories

        mkTiles stories =
            div [ class "storytiles" ] (List.map storyTile stories)

        storyStyle s =
            style [ ( "background", "url(pix/" ++ s.img ++ ")" ), ( "background-size", "cover" ) ]

        storyTile s =
            Html.a [ class "storytile", storyStyle s, Html.Attributes.href (pageToUrl (StoryPage (Maybe.withDefault "1" s.id))) ]
                [ h3 [] [ text s.title ] ]
    in
        [ stories_ ]


tableView : Model -> List (Html Msg)
tableView m =
    let
        c =
            Table.defaultCustomizations

        myThead =
            c.thead
                >> .children
                >> tr []
                >> List.singleton
                >> Table.HtmlDetails []

        tableCustomizations =
            { c | thead = myThead, tableAttrs = [ class "table table-striped" ] }

        tag i s =
            s.tags
                |> List.drop (i - 1)
                |> List.head
                |> Maybe.withDefault ""

        cfg =
            Table.customConfig
                { toId = Maybe.withDefault "" << .id
                , toMsg = SetTableState
                , columns =
                    [ storyTitleColumn
                    , Table.stringColumn "Tag1" (tag 1)
                    , Table.stringColumn "Tag2" (tag 2)
                    , Table.stringColumn "Tag3" (tag 3)
                    , levelColumn
                    ]
                , customizations = tableCustomizations
                }

        -- This is needed to make the level column wide enough so the heading and arrow
        -- don't wrap
        levelColumn : Table.Column Story Msg
        levelColumn =
            Table.veryCustomColumn
                { name = "Level"
                , viewData = \s -> Table.HtmlDetails [ style [ ( "width", "6em" ) ] ] [ Html.text (toString s.level) ]
                , sorter = Table.increasingOrDecreasingBy .level
                }

        storyTitleColumn : Table.Column Story Msg
        storyTitleColumn =
            Table.veryCustomColumn
                { name = "title"
                , viewData = viewStoryLink
                , sorter = Table.increasingOrDecreasingBy .title
                }

        viewStoryLink : Story -> Table.HtmlDetails Msg
        viewStoryLink s =
            Table.HtmlDetails []
                [ Html.a [ Html.Attributes.href (pageToUrl (StoryPage (Maybe.withDefault "1" s.id))) ] [ text s.title ]
                ]
    in
        [ div []
            [ div [ class "form-group" ]
                [ label [ for "storyfilter" ] [ text "Search" ]
                , input
                    [ type_ "text"
                    , value m.storyFilter
                    , onInput StoryFilterInput
                    , id "storyfilter"
                    ]
                    []
                , div [ class "cols-xs-4" ] []
                ]
            ]
        , div [ class "table-responsive" ]
            [ mapStories (Table.view cfg m.tableState << filterStories m.storyFilter) m.stories ]
        ]


filterStories : String -> List Story -> List Story
filterStories storyFilter stories =
    if String.length storyFilter < 3 then
        stories
    else
        let
            r =
                Regex.caseInsensitive (Regex.regex storyFilter)

            match story =
                Regex.contains r story.title || Regex.contains r story.content
        in
            List.filter match stories


viewStory : Model -> String -> Html Msg
viewStory m id_ =
    case m.stories of
        RemoteData.Success stories ->
            case List.filter (\s -> s.id == Just id_) stories of
                s :: _ ->
                    div [ class "panel panel-default" ]
                        [ h2 [] [ text s.title ]
                        , div [ id "storypic" ]
                            [ img [ src ("pix/" ++ s.img) ] []
                            ]
                        , Markdown.toHtml [ id "storycontent" ] s.content
                        , div [ id "storyfooter" ]
                            [ p [] [ text (String.join ", " s.tags), br [] [], text ("Level: " ++ toString s.level) ]
                            ]
                        ]

                _ ->
                    text "Story not found"

        _ ->
            text "Stories have not been loaded"
