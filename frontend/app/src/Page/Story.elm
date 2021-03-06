module Page.Story exposing (Model, Msg, init, subscriptions, update, view)

import AnswersForm
import Api
import Browser.Dom
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Data.Settings exposing (Settings)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Ports
import StoryGraph exposing (StoryGraph)
import Task exposing (Task)
import Util exposing (defaultHttpErrorMsg, printButton, viewIf)
import Views.Answers as Answers
import Views.RobotPanel as RobotPanel
import Views.Story as StoryView
import Views.Words as Words


type alias Model =
    { dictLookup : Maybe Api.DictEntry
    , story : Api.Story
    , storyView : StoryView.State
    , answers : List Api.Answer
    , answersForm : Maybe AnswersForm.Model
    }


type Msg
    = StoryViewMsg StoryView.Msg
    | PrintWindow
    | DictLookup ( String, Int )
    | ClearAnswers
    | AnswersFormMsg AnswersForm.Msg


init : Session -> Int -> Task PageLoadError ( Model, Session )
init originalSession slug =
    let
        handleLoadError e =
            pageLoadError e ("Story is currently unavailable. " ++ defaultHttpErrorMsg e)

        user =
            Session.subjectId originalSession

        mkAnswersForm story answers =
            Maybe.andThen (createFormIfUnanswered story answers) user

        createFormIfUnanswered story answers subId =
            if Session.isTeacher originalSession || List.any (\a -> a.studentId == subId) answers then
                Nothing

            else
                Just (AnswersForm.init story)

        lookupStoryAndCreateModel session =
            case findStoryById (Session.getCache session) slug of
                Just story ->
                    Task.map2
                        (\answers { viewport } ->
                            ( { dictLookup = Nothing
                              , story = story
                              , storyView = StoryView.init (round viewport.width)
                              , answers = answers
                              , answersForm = mkAnswersForm story answers
                              }
                            , session
                            )
                        )
                        (lookupAnswers session story)
                        Browser.Dom.getViewport

                Nothing ->
                    Task.fail
                        << PageLoadError
                    <|
                        "Sorry. That story couldn't be found."
                            ++ (case user of
                                    Just _ ->
                                        ""

                                    Nothing ->
                                        " You probably need to sign in to see it."
                               )

        lookupAnswers session story =
            if Session.isStudent session || Session.isTeacher session then
                Api.getSchoolAnswers (authorization session) (Just story.id) Nothing
                    |> Http.toTask
                    |> Task.mapError handleLoadError

            else
                Task.succeed []
    in
    Session.loadDictionary originalSession
        |> Task.andThen Session.loadStories
        |> Task.mapError handleLoadError
        |> Task.andThen lookupStoryAndCreateModel


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ Sub.map StoryViewMsg StoryView.subscriptions, Ports.dictLookup DictLookup ]


view : Session -> Model -> { title : String, content : Html Msg }
view session m =
    let
        cache =
            Session.getCache session
    in
    { title = m.story.title
    , content =
        div []
            [ viewIf (Session.isTeacher session) (div [ class "print:none mb-2" ] [ printButton PrintWindow "Print this story" ])
            , Html.map StoryViewMsg <| StoryView.view (Session.getSettings session) m.story cache.storyGraph m.storyView
            , m.dictLookup
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
                |> Words.view cache.dict
            , viewIf (Session.isStudent session) (viewAnswersForm m)
            , viewIf (Session.isTeacher session) (viewPrintAnswerSections m.story)
            , viewIf (m.answersForm == Nothing && not (List.isEmpty m.answers))
                (div [ class "hidden-print mt-8" ]
                    (h2 [ class "text-lg text-center font-bold mb-2" ] [ text "Story answers" ] :: Answers.view m.story m.answers)
                )
            ]
    }


viewAnswersForm : Model -> Html Msg
viewAnswersForm m =
    case m.answersForm of
        Nothing ->
            div [] []

        Just f ->
            Html.map AnswersFormMsg <|
                div [ id "activities", class "mt-8" ]
                    [ h2 [ class "text-lg text-gray-700" ] [ text "Answers" ]
                    , AnswersForm.view f
                    ]


viewPrintAnswerSections : Api.Story -> Html Msg
viewPrintAnswerSections story =
    let
        cls =
            class "text-base font-bold mb-24"
    in
    div [ id "printactivities", class "mt-2" ]
        [ h2 [ cls ] [ text "Connect this story with yourself or something you know about." ]
        , h2 [ cls ] [ text "Think of a question the story makes you want to ask." ]
        , h2 [ cls ] [ text "Write one sentence that captures the main idea." ]
        , h2 [ cls ] [ text ("What do you think the word \"" ++ story.clarifyWord ++ "\" means?") ]
        ]


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Session )
update session msg model =
    case msg of
        StoryViewMsg svm ->
            let
                ( newStoryView, cmd ) =
                    StoryView.update svm model.storyView
            in
            ( ( { model | storyView = newStoryView }, Cmd.map StoryViewMsg cmd ), session )

        DictLookup ( w, i ) ->
            ( ( { model | dictLookup = Just (Api.DictEntry w i) }, Cmd.none ), session )

        PrintWindow ->
            ( ( model, Ports.printWindow () ), session )

        ClearAnswers ->
            ( ( resetAnswersForm model, Cmd.none ), session )

        AnswersFormMsg subMsg ->
            case Maybe.map (AnswersForm.update session subMsg) model.answersForm of
                Nothing ->
                    ( ( model, Cmd.none ), session )

                Just ( ( subModel, cmd ), Nothing ) ->
                    ( ( { model | answersForm = Just subModel }, Cmd.map AnswersFormMsg cmd ), session )

                Just ( ( _, cmd ), Just submittedAnswer ) ->
                    storyCompleted session cmd submittedAnswer model


storyCompleted : Session -> Cmd AnswersForm.Msg -> Api.Answer -> Model -> ( ( Model, Cmd Msg ), Session )
storyCompleted session cmd answer model =
    let
        newModel =
            { model | answersForm = Nothing, answers = answer :: model.answers }
    in
    ( ( newModel, Cmd.map AnswersFormMsg cmd ), Session.storyCompleted session answer )


resetAnswersForm : Model -> Model
resetAnswersForm m =
    case m.answersForm of
        Nothing ->
            m

        Just f ->
            { m | answersForm = Just (AnswersForm.init (.story f)) }
