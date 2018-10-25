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
import Validate exposing (Validator, validate, ifBlank, ifFalse)
import Views.Form as Form
import Views.Story as StoryView


type alias Model =
    { errors : List String
    , story : Api.Story
    , stories : Zipper Api.Story
    , storyView : StoryView.State
    , qualificationTags : List String
    , curriculumTags : List String
    , allTags : List String
    , tagsSelect : Select.State
    , curriculumSelect : Select.State
    , qualificationSelect : Select.State
    }


type Msg
    = ContentInput String
    | StoryViewMsg StoryView.Msg
    | Save
    | SaveResponse (Result Http.Error Api.Story)
    | CancelChanges
    | TagSelectMsg (Select.Msg String)
    | CurriculumSelectMsg (Select.Msg String)
    | QualificationSelectMsg (Select.Msg String)
    | OnTagSelect (Maybe String)
    | OnCurriculumSelect (Maybe String)
    | OnQualificationSelect (Maybe String)
    | OnRemoveTag String
    | SetTitle String
    | ToggleEnabled
    | IncrementLevel
    | DecrementLevel
    | SetClarifyWord String
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
                            { errors = []
                            , story = Zipper.current zipper
                            , stories = zipper
                            , storyView = StoryView.init (round viewport.width)
                            , qualificationTags = makeTags (List.filterMap .qualification session.cache.stories)
                            , curriculumTags = makeTags (List.filterMap .curriculum session.cache.stories)
                            , allTags = makeTags (List.concatMap .tags session.cache.stories)
                            , tagsSelect = Select.newState "Tags"
                            , curriculumSelect = Select.newState "Curriculum"
                            , qualificationSelect = Select.newState "Qualification"
                            }
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
                        |> Task.map (\m -> ( m, newSession ))
                )


makeZipper : List Api.Story -> Int -> Maybe (Zipper Api.Story)
makeZipper stories storyId =
    Zipper.fromList stories
        |> Maybe.andThen (Zipper.findFirst (\s -> s.id == storyId))


makeTags : List String -> List String
makeTags =
    Set.fromList >> Set.toList


selectConfig : (Maybe String -> Msg) -> Select.Config Msg String
selectConfig msg =
    Select.newConfig msg identity
        |> Select.withCutoff 12
        |> Select.withInputWrapperStyles
            [ ( "padding", "0.4rem" ) ]
        |> Select.withItemStyles [ ( "font-size", "1rem" ) ]
        |> Select.withMenuStyles [ ( "background", "white" ) ]
        |> Select.withNotFound "No matches"
        |> Select.withNotFoundStyles [ ( "padding", "0 2rem" ) ]
        |> Select.withHighlightedItemStyles []


curriculumSelectConfig : Select.Config Msg String
curriculumSelectConfig =
    selectConfig OnCurriculumSelect
        |> Select.withPrompt "Select a curriculum"


qualificationSelectConfig : Select.Config Msg String
qualificationSelectConfig =
    selectConfig OnQualificationSelect
        |> Select.withPrompt "Select a qualification"


tagSelectConfig : Select.Config Msg String
tagSelectConfig =
    selectConfig OnTagSelect
        |> Select.withOnRemoveItem OnRemoveTag
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

        CancelChanges ->
            ( { model | story = Zipper.current model.stories, errors = [] }, Cmd.none )

        ToggleEnabled ->
            updateStory (\s -> { s | enabled = not s.enabled } ) model

        ContentInput newContent ->
            updateStory (\s -> { s | content = newContent }) model

        SetTitle newTitle ->
            updateStory (\s -> { s | title = newTitle }) model

        IncrementLevel ->
            updateStory (\s -> { s | level = if model.story.level < 9 then model.story.level + 1 else 9 }) model

        DecrementLevel ->
            updateStory (\s -> { s | level = if model.story.level > 0 then model.story.level - 1 else 0 }) model

        SetClarifyWord newClarifyWord ->
            updateStory (\s -> { s | clarifyWord = newClarifyWord }) model

        Save ->
            case validate validator model.story of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none )

                _ ->
                    ( { model | errors = [] }
                    , (Api.postStoriesByStoryId (authorization session) model.story.id model.story
                        |> Http.send SaveResponse
                      )
                    )

        SaveResponse (Ok story) ->
            ( { model | errors = [], story = story, stories = Zipper.mapCurrent (\_ -> story) model.stories }, Cmd.none )

        SaveResponse (Err e) ->
            ( { model | errors = [ defaultHttpErrorMsg e ] }, Cmd.none )

        OnTagSelect maybeTag ->
            let
                selectedTags =
                    maybeTag
                        |> Maybe.map (List.singleton >> List.append model.story.tags)
                        |> Maybe.withDefault []
            in
            updateStory (\s -> { s | tags = selectedTags }) model

        OnCurriculumSelect newCurriculum ->
            updateStory (\s -> { s | curriculum = newCurriculum }) model

        OnQualificationSelect newQualification ->
            updateStory (\s -> { s | qualification = newQualification }) model

        OnRemoveTag tagToRemove ->
            let
                selectedTags =
                    List.filter (\curTag -> curTag /= tagToRemove) model.story.tags
            in
            updateStory (\s -> { s | tags = selectedTags }) model

        TagSelectMsg sub ->
            let
                ( updated, cmd ) =
                    Select.update tagSelectConfig sub model.tagsSelect
            in
                ( { model | tagsSelect = updated }, cmd )

        CurriculumSelectMsg sub ->
            let
                ( updated, cmd ) =
                    Select.update curriculumSelectConfig sub model.curriculumSelect
            in
            ( { model | curriculumSelect = updated }, cmd )


        QualificationSelectMsg sub ->
            let
                ( updated, cmd ) =
                    Select.update qualificationSelectConfig sub model.qualificationSelect
            in
            ( { model | qualificationSelect = updated }, cmd )

        Next ->
            updateZipper Zipper.next model

        Previous ->
            updateZipper Zipper.previous model


updateStory : (Api.Story -> Api.Story) -> Model -> (Model, Cmd msg)
updateStory f model =
    ( { model | story = f model.story }, Cmd.none )


updateZipper : (Zipper Api.Story -> Zipper Api.Story) -> Model -> (Model, Cmd msg)
updateZipper f model =
    let
        newStories = f model.stories
    in
    ( { model | story = Zipper.current newStories, stories = newStories, errors = [] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StoryViewMsg StoryView.subscriptions
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
                    [ Html.map TagSelectMsg
                        ( Select.viewMulti tagSelectConfig
                              model.tagsSelect
                              model.allTags
                              model.story.tags
                        )
                    ]
                , div [ class "col-md-3" ]
                    [ button
                        [ class "btn btn-default"
                        , onClick Save
                        , disabled (model.story == Zipper.current model.stories)
                        ]
                        [ text "Save Changes" ]
                    ]
                , div [ class "col-md-3" ]
                    [ button
                        [ class "btn btn-default"
                        , onClick CancelChanges
                        , disabled (model.story == Zipper.current model.stories)
                        ]
                        [ text "Cancel Changes" ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-6" ]
                    [ Html.map CurriculumSelectMsg
                        ( Select.view curriculumSelectConfig
                              model.curriculumSelect
                              model.curriculumTags
                              model.story.curriculum
                        )
                    ]
                , div [ class "col-md-6" ]
                    [ Html.map QualificationSelectMsg
                        ( Select.view qualificationSelectConfig
                              model.qualificationSelect
                              model.qualificationTags
                              model.story.qualification
                        )
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-6" ]
                    [ Form.input
                        [ class "form-control-lg"
                        , placeholder "Title"
                        , value model.story.title
                        , onInput SetTitle
                        ]
                        []
                    ]
                , div [ class "col-md-3" ]
                    [ Form.input
                        [ class "form-control-lg"
                        , placeholder "Clarify word"
                        , value model.story.clarifyWord
                        , onInput SetClarifyWord
                        ]
                        []
                    ]
                , div [ class "col-md-1", style "display" "flex", style "align-items" "center"]
                    [ button [ class "btn btn-small",  onClick DecrementLevel ] [ text "-" ]
                    , div [ style "padding-left" "0.5em", style "padding-right" "0.5em"]
                        [ text (String.fromInt model.story.level)
                        ]
                    , button [ class "btn btn-small", onClick IncrementLevel ] [ text "+" ]
                    ]
                , div [ class "col-md-2" ]
                    [ button [ class "btn pull-right", onClick ToggleEnabled ]
                        [ text
                            (if model.story.enabled then
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
                        [ value model.story.content
                        , class "form-control"
                        , onInput ContentInput
                        , rows 30
                        ]
                        []
                    ]
                , div [ class "col-md-6" ]
                    [ Html.map StoryViewMsg <| StoryView.view defaultSettings model.story model.storyView ]
                ]
            ]
    }


type alias Error = String


validator : Validator Error Api.Story
validator =
    Validate.all
        [ ifBlank .content "You must enter some content for the story"
        , ifBlank .clarifyWord "You must enter a clarify word for the story"
        , ifBlank .title "You must enter a title for the story"
        , ifFalse clarifyWordExists "The story doesn't contain the clarify word"
        ]


clarifyWordExists : Api.Story -> Bool
clarifyWordExists s =
    let
        cw = String.toLower s.clarifyWord
        isPhrase = String.contains " " cw
        inContent = String.contains cw (String.toLower s.content)
        inTitle = String.contains cw (String.toLower s.title)
    in
    isPhrase || inContent || inTitle
