module Page.Editor exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Data.Settings exposing (defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import List.InfiniteZipper as Zipper exposing (InfiniteZipper)
import Multiselect
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Set
import Task exposing (Task)
import Tuple exposing (second)
import Util exposing ((=>), defaultHttpErrorMsg, onClickPreventDefault)
import Views.Form as Form
import Views.Story as StoryView
import Window


type alias Model =
    { errors : List String
    , stories : InfiniteZipper Api.Story
    , storyView : StoryView.State
    , sqaTags : List String
    , tagsMultiselect : Multiselect.Model
    }


type Msg
    = ContentInput String
    | StoryViewMsg StoryView.Msg
    | Save
    | SaveResponse (Result Http.Error Api.Story)
    | MSMsg Multiselect.Msg
    | Next
    | Previous


init : Session -> Int -> Task PageLoadError ( Model, Session )
init originalSession slug =
    let
        handleLoadError e =
            pageLoadError e ("Story is currently unavailable. " ++ defaultHttpErrorMsg e)

        lookupStoryAndCreateModel session =
            case makeZipper session.cache.stories slug of
                Just zipper ->
                    Task.map
                        (\size ->
                            (Model []
                                zipper
                                (StoryView.init size)
                                (makeSqaTags session.cache.stories)
                                (initMultiselect session.cache.stories)
                            )
                        )
                        Window.size

                Nothing ->
                    Task.fail (PageLoadError "Sorry. That story couldn't be found.")
    in
        Session.loadDictionary originalSession
            |> Task.andThen (\newSession -> Session.loadStories newSession)
            |> Task.mapError handleLoadError
            |> Task.andThen
                (\newSession ->
                    lookupStoryAndCreateModel newSession
                        |> Task.map (updateZipper identity)
                        |> Task.map (\m -> ( m, newSession ))
                )


makeZipper : List Api.Story -> Int -> Maybe (InfiniteZipper Api.Story)
makeZipper stories storyId =
    Zipper.fromList stories
        |> Maybe.andThen (Zipper.findFirst (\s -> s.id == storyId))


makeSqaTags : List Api.Story -> List String
makeSqaTags =
    List.map .qualification >> Set.fromList >> Set.toList


initMultiselect : List Api.Story -> Multiselect.Model
initMultiselect stories =
    let
        tags =
            stories
                |> List.concatMap .tags
                |> Set.fromList
                |> Set.toList
                |> \ts -> List.map2 (,) ts ts
    in
        Multiselect.initModel tags "Tags"


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        StoryViewMsg svm ->
            let
                ( newStoryView, cmd ) =
                    StoryView.update svm model.storyView
            in
                { model | storyView = newStoryView } => Cmd.map StoryViewMsg cmd

        ContentInput newContent ->
            { model | stories = Zipper.mapCurrent (\s -> { s | content = newContent }) model.stories }
                => Cmd.none

        Save ->
            model
                => (Api.postStoriesByStoryId (authorization session) (.id <| Zipper.current model.stories) (Zipper.current model.stories)
                        |> Http.send SaveResponse
                   )

        SaveResponse (Ok story) ->
            { model | errors = [], stories = Zipper.mapCurrent (\_ -> story) model.stories } => Cmd.none

        SaveResponse (Err e) ->
            { model | errors = [ defaultHttpErrorMsg e ] }
                => Cmd.none

        MSMsg sub ->
            let
                ( subModel, subCmd ) =
                    Multiselect.update sub model.tagsMultiselect

                selected =
                    List.map second (Multiselect.getSelectedValues subModel)

                newZipper =
                    Zipper.mapCurrent (\s -> { s | tags = selected }) model.stories
            in
                { model | tagsMultiselect = subModel, stories = newZipper } ! [ Cmd.map MSMsg subCmd ]

        Next ->
            updateZipper Zipper.next model => Cmd.none

        Previous ->
            updateZipper Zipper.previous model => Cmd.none


updateZipper : (InfiniteZipper Api.Story -> InfiniteZipper Api.Story) -> Model -> Model
updateZipper f model =
    let
        newStories =
            f model.stories

        story =
            Zipper.current newStories

        ms =
            model.tagsMultiselect

        selection =
            List.map2 (,) story.tags story.tags
    in
        { model | stories = newStories, tagsMultiselect = { ms | selected = selection } }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StoryViewMsg StoryView.subscriptions
        , Sub.map MSMsg <| Multiselect.subscriptions model.tagsMultiselect
        ]


view : Model -> Html Msg
view model =
    div [ id "editor", class "container page" ]
        [ nav [ class "row" ]
            [ ul [ class "pager" ]
                [ li [ class "previous" ] [ a [ href "#", onClickPreventDefault Previous ] [ text "Prev" ] ]
                , li [ class "next" ] [ a [ class "pull-right", href "#", onClickPreventDefault Next ] [ text "Next" ] ]
                ]
            ]
        , div [ class "row" ]
            [ Form.viewErrorMsgs model.errors
            , div [ class "col-md-6" ]
                [ Html.map MSMsg <| Multiselect.view model.tagsMultiselect
                ]
            , div [ class "col-md-6" ]
                [ button [ class "btn btn-default", onClick Save ] [ text "Save Changes" ]
                ]
            ]
        , div [ class "row panes" ]
            [ div [ class "col-md-6 contentinput" ]
                [ textarea
                    [ value (.content (Zipper.current model.stories))
                    , class "form-control"
                    , onInput ContentInput
                    , rows 30
                    ]
                    []
                ]
            , div [ class "col-md-6" ]
                [ Html.map StoryViewMsg <| StoryView.view defaultSettings (Zipper.current model.stories) model.storyView ]
            ]
        ]
