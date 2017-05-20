module Page.Story exposing (Model, Msg, init, subscriptions, update, view)

import AnswersForm
import Api
import Data.Session as Session exposing (Session)
import Exts.List exposing (firstMatch)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Ports
import Task exposing (Task)
import Util exposing ((=>), viewIf)
import Views.Page as Page
import Views.RobotPanel as RobotPanel
import Views.Story as Story
import Views.Words as Words


type alias Model =
    { picWidth : Int
    , dictLookup : Maybe Api.DictEntry
    , story : Api.Story
    , answersForm : Maybe AnswersForm.Model
    }


type Msg
    = GetImgWidth String
    | ImageWidth Float
    | DictLookup ( String, Int )
    | ClearAnswers
    | AnswersFormMsg AnswersForm.Msg


init : Session -> String -> Task PageLoadError ( Model, Session )
init session slug =
    let
        handleLoadError _ =
            pageLoadError Page.Other "Story is currently unavailable."

        -- TODO: Check user type and whether user has already completed this story
        mkAnswersForm story =
            Just (AnswersForm.init story)

        lookupStoryAndCreateModel session =
            case firstMatch (\s -> s.id == slug) session.stories of
                Just story ->
                    Task.succeed ( Model 0 Nothing story (mkAnswersForm story), session )

                Nothing ->
                    Task.fail (pageLoadError Page.Other "Sorry. That story couldn't be found.")
    in
        Session.loadDictionary session
            |> Task.andThen (\newSession -> Session.loadStories newSession)
            |> Task.mapError handleLoadError
            |> Task.andThen lookupStoryAndCreateModel


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ Ports.imgWidth ImageWidth, Ports.dictLookup DictLookup ]


view : Session -> Model -> Html Msg
view session m =
    div [ class "container page" ]
        [ RobotPanel.view
        , Story.view m.story m.picWidth GetImgWidth
        , m.dictLookup
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
            |> Words.view session.dict
        , viewIf (Session.isStudent session) (viewAnswersForm m)
        ]


viewAnswersForm : Model -> Html Msg
viewAnswersForm m =
    case m.answersForm of
        Nothing ->
            div [] []

        Just f ->
            Html.map (AnswersFormMsg) <|
                div [ id "activities" ]
                    [ h2 [] [ text "Answers" ]
                    , AnswersForm.view f
                    ]


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        GetImgWidth s ->
            model => Ports.getImgWidth s

        ImageWidth w ->
            { model | picWidth = round w } => Cmd.none

        DictLookup ( w, i ) ->
            { model | dictLookup = Just (Api.DictEntry w i) } => Cmd.none

        ClearAnswers ->
            resetAnswersForm model => Cmd.none

        AnswersFormMsg subMsg ->
            case Maybe.map (AnswersForm.update session subMsg) model.answersForm of
                Nothing ->
                    model => Cmd.none

                Just ( ( subModel, cmd ), Nothing ) ->
                    { model | answersForm = Just subModel } => Cmd.map AnswersFormMsg cmd

                Just ( ( _, cmd ), submittedAnswer ) ->
                    { model | answersForm = Nothing } => Cmd.map AnswersFormMsg cmd


resetAnswersForm : Model -> Model
resetAnswersForm m =
    case m.answersForm of
        Nothing ->
            m

        Just f ->
            { m | answersForm = Just (AnswersForm.init (.story f)) }
