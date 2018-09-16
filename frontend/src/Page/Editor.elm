module Page.Editor exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Browser.Dom
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Data.Settings exposing (defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import List.Zipper.Infinite as Zipper exposing (Zipper)
import Select
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Set
import Task exposing (Task)
import Tuple exposing (pair, second)
import Util exposing (defaultHttpErrorMsg)
import Views.Form as Form
import Views.Story as StoryView


type alias Model =
    { errors : List String
    , stories : Zipper Api.Story
    , storyView : StoryView.State
    , sqaTags : List String
    , allTags : List String
    , selectedTags : List String
    , tagsSelect : Select.State
    }


type Msg
    = ContentInput String
    | StoryViewMsg StoryView.Msg
    | Save
    | SaveResponse (Result Http.Error Api.Story)
    | SelectMsg (Select.Msg String)
    | OnSelect (Maybe String)
    | OnRemoveItem String
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
                        (\{ viewport } ->
                            (Model []
                                zipper
                                (StoryView.init (round viewport.width))
                                (makeSqaTags session.cache.stories)
                                (makeTags session.cache.stories)
                                []
                                (Select.newState "Tags")
                            )
                        )
                        Browser.Dom.getViewport

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


makeZipper : List Api.Story -> Int -> Maybe (Zipper Api.Story)
makeZipper stories storyId =
    Zipper.fromList stories
        |> Maybe.andThen (Zipper.findFirst (\s -> s.id == storyId))


makeSqaTags : List Api.Story -> List String
makeSqaTags =
    List.filterMap .qualification >> Set.fromList >> Set.toList


makeTags : List Api.Story -> List String
makeTags stories =
    stories
        |> List.concatMap .tags
        |> Set.fromList
        |> Set.toList


selectConfig : Select.Config Msg String
selectConfig =
    Select.newConfig OnSelect identity
        |> Select.withOnRemoveItem OnRemoveItem
        |> Select.withCutoff 12
        |> Select.withInputWrapperStyles
            [ ( "padding", "0.4rem" ) ]
        |> Select.withItemStyles [ ( "font-size", "1rem" ) ]
        |> Select.withMenuStyles [ ( "background", "white" ) ]
        |> Select.withNotFound "No matches"
        |> Select.withNotFoundStyles [ ( "padding", "0 2rem" ) ]
        |> Select.withHighlightedItemStyles []
        |> Select.withPrompt "Select a tag"


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        StoryViewMsg svm ->
            let
                ( newStoryView, cmd ) =
                    StoryView.update svm model.storyView
            in
                ( { model | storyView = newStoryView }, Cmd.map StoryViewMsg cmd )

        ContentInput newContent ->
            ( { model | stories = Zipper.mapCurrent (\s -> { s | content = newContent }) model.stories }
            , Cmd.none
            )

        Save ->
            ( model
            , (Api.postStoriesByStoryId (authorization session) (.id <| Zipper.current model.stories) (Zipper.current model.stories)
                |> Http.send SaveResponse
              )
            )

        SaveResponse (Ok story) ->
            ( { model | errors = [], stories = Zipper.mapCurrent (\_ -> story) model.stories }, Cmd.none )

        SaveResponse (Err e) ->
            ( { model | errors = [ defaultHttpErrorMsg e ] }, Cmd.none )

        OnSelect maybeTag ->
            let
                selectedTags =
                    maybeTag
                        |> Maybe.map (List.singleton >> List.append model.selectedTags)
                        |> Maybe.withDefault []
            in
                updateTags model selectedTags

        OnRemoveItem tagToRemove ->
            let
                selectedTags =
                    List.filter (\curTag -> curTag /= tagToRemove)
                        model.selectedTags
            in
                updateTags model selectedTags

        SelectMsg sub ->
            let
                ( updated, cmd ) =
                    Select.update selectConfig sub model.tagsSelect
            in
                ( { model | tagsSelect = updated }, cmd )

        Next ->
            ( updateZipper Zipper.next model, Cmd.none )

        Previous ->
            ( updateZipper Zipper.previous model, Cmd.none )


updateTags : Model -> List String -> (Model, Cmd msg)
updateTags model newTags =
    let
        newZipper =
            Zipper.mapCurrent (\s -> { s | tags = newTags }) model.stories

    in
        ( { model | selectedTags = newTags, stories = newZipper }, Cmd.none )


updateZipper : (Zipper Api.Story -> Zipper Api.Story) -> Model -> Model
updateZipper f model =
    let
        newStories =
            f model.stories

        story =
            Zipper.current newStories

    in
        { model | stories = newStories, selectedTags = story.tags }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StoryViewMsg StoryView.subscriptions
        -- , Sub.map SelectMsg <| Select.subscriptions model.tagsSelect
        ]


view : Model -> { title: String, content: Html Msg }
view model =
    { title = "Story editor"
    , content =
        div [ id "editor", class "container page" ]
            [ nav [ class "row" ]
                [ ul [ class "pager" ]
                    [ li [ class "previous" ] [ a [ href "#", onClick Previous ] [ text "Prev" ] ]
                    , li [ class "next" ] [ a [ class "pull-right", href "#", onClick Next ] [ text "Next" ] ]
                    ]
                ]
            , div [ class "row" ]
                [ Form.viewErrorMsgs model.errors
                , div [ class "col-md-6" ]
                    [ Html.map SelectMsg
                        ( Select.viewMulti selectConfig
                              model.tagsSelect
                              model.allTags
                              model.selectedTags
                        )
                    ]
                , div [ class "col-md-6" ]
                    [ button [ class "btn btn-default", onClick Save ] [ text "Save Changes" ]
                    , text " "
                    , button [ class "btn" ]
                        [ text
                            (if .enabled (Zipper.current model.stories) then
                                "Enabled"
                            else
                                "Disabled"
                            )
                        ]
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
    }
