module Page.Editor exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Browser.Dom
import Components
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Data.Settings exposing (defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import List.Zipper.Infinite as Zipper exposing (Zipper)
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Select
import Set
import Task exposing (Task)
import Tuple exposing (pair, second)
import Util exposing (defaultHttpErrorMsg)
import Validate exposing (Validator, ifBlank, ifFalse, validate)
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

        lookupStoryAndCreateModel cache =
            case makeZipper cache.stories slug of
                Just zipper ->
                    Task.map
                        (\{ viewport } ->
                            { errors = []
                            , story = Zipper.current zipper
                            , stories = zipper
                            , storyView = StoryView.init (round viewport.width)
                            , qualificationTags = makeTags (List.filterMap .qualification cache.stories)
                            , curriculumTags = makeTags (List.filterMap .curriculum cache.stories)
                            , allTags = makeTags (List.concatMap .tags cache.stories)
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
        |> Task.andThen Session.loadStories
        |> Task.mapError handleLoadError
        |> Task.andThen
            (\newSession ->
                lookupStoryAndCreateModel (Session.getCache newSession)
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
    let
        strContainsFilter query items =
            if String.length query < 2 then
                Nothing

            else
                items
                    |> List.filter (\item -> String.contains (String.toLower query) (String.toLower item))
                    |> Just
    in
    Select.newConfig
        { onSelect = msg
        , toLabel = identity
        , filter = strContainsFilter
        }
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
        |> Select.withInputId "story_curriculum"
        |> Select.withPrompt "Select a curriculum"


qualificationSelectConfig : Select.Config Msg String
qualificationSelectConfig =
    selectConfig OnQualificationSelect
        |> Select.withInputId "story_qualification"
        |> Select.withPrompt "Select a qualification"


tagSelectConfig : Select.Config Msg String
tagSelectConfig =
    selectConfig OnTagSelect
        |> Select.withInputId "story_tags"
        |> Select.withOnRemoveItem OnRemoveTag
        |> Select.withPrompt "Select a tag"
        |> Select.withMultiSelection True


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
            updateStory (\s -> { s | enabled = not s.enabled }) model

        ContentInput newContent ->
            updateStory (\s -> { s | content = newContent }) model

        SetTitle newTitle ->
            updateStory (\s -> { s | title = newTitle }) model

        IncrementLevel ->
            updateStory
                (\s ->
                    { s
                        | level =
                            if model.story.level < 9 then
                                model.story.level + 1

                            else
                                9
                    }
                )
                model

        DecrementLevel ->
            updateStory
                (\s ->
                    { s
                        | level =
                            if model.story.level > 0 then
                                model.story.level - 1

                            else
                                0
                    }
                )
                model

        SetClarifyWord newClarifyWord ->
            updateStory (\s -> { s | clarifyWord = newClarifyWord }) model

        Save ->
            case validate validator model.story of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none )

                _ ->
                    ( { model | errors = [] }
                    , Api.postStoriesByStoryId (authorization session) model.story.id model.story
                        |> Http.send SaveResponse
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


updateStory : (Api.Story -> Api.Story) -> Model -> ( Model, Cmd msg )
updateStory f model =
    ( { model | story = f model.story }, Cmd.none )


updateZipper : (Zipper Api.Story -> Zipper Api.Story) -> Model -> ( Model, Cmd msg )
updateZipper f model =
    let
        newStories =
            f model.stories
    in
    ( { model | story = Zipper.current newStories, stories = newStories, errors = [] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StoryViewMsg StoryView.subscriptions
        ]


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Story editor"
    , content =
        div [ id "editor", class "max-w-2xl mx-auto" ]
            [ div [ class "flex flex-wrap justify-between mb-3" ]
                [ Components.link [ href "#", onClick Previous ] "Prev"
                , Components.toolbar
                    [ ( Save, model.story == Zipper.current model.stories, "Save Changes" )
                    , ( CancelChanges, model.story == Zipper.current model.stories, "Cancel Changes" )
                    ]
                    []
                , Components.link [ href "#", onClick Next ] "Next"
                ]
            , Form.viewErrorMsgs model.errors
            , div [ class "flex flex-wrap items-center justify-between my-2" ]
                [ div [ class "flex items-center pr-1" ]
                    [ Form.label [ class "mr-2", for "story_title" ] [ text "Title" ]
                    , Form.input
                        [ id "story_title"
                        , placeholder "Title"
                        , value model.story.title
                        , onInput SetTitle
                        ]
                        []
                    ]
                , div [ class "flex items-center pr-1" ]
                    [ Form.label [ class "mr-2", for "clarify_word" ] [ text "Clarify word" ]
                    , Form.input
                        [ id "clarify_word"
                        , placeholder "Clarify word"
                        , value model.story.clarifyWord
                        , onInput SetClarifyWord
                        ]
                        []
                    ]
                , div [ class "flex items-center my-2 pr-1" ]
                    [ Form.label [ class "mr-2", for "story_level" ] [ text "Level" ]
                    , Components.btnSmall [ onClick DecrementLevel ] [ text "-" ]
                    , div [ id "story_level", class "px-1" ]
                        [ text (String.fromInt model.story.level)
                        ]
                    , Components.btnSmall [ onClick IncrementLevel ] [ text "+" ]
                    ]
                , div [ class "flex items-center my-2 pr-1" ]
                    [ Form.label [ class "mr-2", for "story_tags" ] [ text "Tags" ]
                    , Html.map TagSelectMsg
                        (Select.view tagSelectConfig
                            model.tagsSelect
                            model.allTags
                            model.story.tags
                        )
                    ]
                , div [ class "flex items-center my-2 pr-1" ]
                    [ Form.label [ class "mr-2", for "story_curriculum" ] [ text "Curriculum" ]
                    , Html.map CurriculumSelectMsg
                        (Select.view curriculumSelectConfig
                            model.curriculumSelect
                            model.curriculumTags
                            (model.story.curriculum
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []
                            )
                        )
                    ]
                , div [ class "flex items-center my-2 pr-1" ]
                    [ Form.label [ class "mr-2", for "story_qualification" ] [ text "Qualification" ]
                    , Html.map QualificationSelectMsg
                        (Select.view qualificationSelectConfig
                            model.qualificationSelect
                            model.qualificationTags
                            (model.story.qualification
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []
                            )
                        )
                    ]
                , Form.checkbox ToggleEnabled model.story.enabled "Enabled"
                ]
            , div [ class "flex mt-2" ]
                [ div [ class "flex-1 px-2 mr-2" ]
                    [ Form.textarea
                        [ value model.story.content
                        , onInput ContentInput
                        , class "w-full"
                        , rows 30
                        ]
                        []
                    ]
                , div [ class "flex-1 mx-2" ]
                    [ Html.map StoryViewMsg <| StoryView.view Nothing model.story model.storyView ]
                ]
            ]
    }


type alias Error =
    String


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
        cw =
            String.toLower s.clarifyWord

        isPhrase =
            String.contains " " cw

        inContent =
            String.contains cw (String.toLower s.content)

        inTitle =
            String.contains cw (String.toLower s.title)
    in
    isPhrase || inContent || inTitle
