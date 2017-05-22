module AnswersForm exposing (Msg, Model, init, update, view)

import Api
import Bootstrap exposing (errorClass, submitButton)
import Data.Session exposing (Session, authorization)
import Dict
import Drawer exposing (DrawerType(..))
import Form exposing (Form)
import Form.Input as Input
import Form.Validate as Validate exposing (Validation, field, nonEmpty, string, succeed)
import Html exposing (Html, button, div, em, text, label)
import Html.Attributes exposing (id, class, for, type_)
import Html.Events exposing (onClick)
import Http
import Util exposing ((=>), formCompleted)


type ClarifyMethod
    = ReadAround
    | BreakDown
    | Substitution


type Msg
    = ToggleDrawer DrawerType
    | FormMsg Form.Msg
    | SubmitAnswersResponse (Result Http.Error Api.Answer)


type CustomError
    = InvalidClarifyMethod


type alias Answers =
    { connectAnswer : String
    , questionAnswer : String
    , summary : String
    , clarification : String
    , clarificationMethod : ClarifyMethod
    }


type alias Model =
    { story : Api.Story
    , showDrawer : Maybe DrawerType
    , form : Form CustomError Answers
    }


init : Api.Story -> Model
init s =
    { story = s, showDrawer = Nothing, form = Form.initial [] answerFormValidation }


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Maybe Api.Answer )
update session msg model =
    case msg of
        ToggleDrawer d ->
            if model.showDrawer == Just d then
                { model | showDrawer = Nothing }
                    => Cmd.none
                    => Nothing
            else
                { model | showDrawer = Just d }
                    => Cmd.none
                    => Nothing

        FormMsg fMsg ->
            case formCompleted fMsg model.form of
                Just answers ->
                    model
                        => submitAnswers session model.story answers
                        => Nothing

                Nothing ->
                    { model | form = Form.update answerFormValidation fMsg model.form }
                        => Cmd.none
                        => Nothing

        SubmitAnswersResponse (Ok answer) ->
            model
                => Cmd.none
                => Just answer

        SubmitAnswersResponse (Err _) ->
            model => Cmd.none => Nothing


submitAnswers : Session -> Api.Story -> Answers -> Cmd Msg
submitAnswers session story answers =
    let
        answer =
            Api.Answer "" story.id "" answers.connectAnswer answers.questionAnswer answers.summary answers.clarification
    in
        Api.postSchoolAnswers (authorization session) answer
            |> Http.send SubmitAnswersResponse


answerFormValidation : Validation CustomError Answers
answerFormValidation =
    let
        nonEmptyString =
            string |> Validate.andThen nonEmpty

        options =
            Dict.fromList
                [ ( toString ReadAround, ReadAround )
                , ( toString BreakDown, BreakDown )
                , ( toString Substitution, Substitution )
                ]

        validateClarifyMethod =
            Validate.customValidation
                string
                (\s ->
                    case Dict.get s options of
                        Just cm ->
                            Ok cm

                        Nothing ->
                            Err (Validate.customError InvalidClarifyMethod)
                )
    in
        Validate.map5 Answers
            (field "connect" nonEmptyString)
            (field "question" nonEmptyString)
            (field "summarise" nonEmptyString)
            (field "clarify" nonEmptyString)
            (field "clarifyMethod" validateClarifyMethod)


view : Model -> Html Msg
view m =
    div []
        [ viewForm m
        , Drawer.view (.showDrawer m) ToggleDrawer
        ]


viewForm : Model -> Html Msg
viewForm m =
    let
        answerField nm lbl =
            Form.getFieldAsString nm m.form
                |> \fld ->
                    div [ class (errorClass fld.liveError) ]
                        [ label [ for (nm ++ "Input") ] lbl
                        , Input.textArea fld [ class "form-control", id (nm ++ "Input") ]
                            |> Html.map FormMsg
                        ]

        clarifyMethodOptions =
            [ ( "", "Please choose one" )
            , ( toString ReadAround, "Read a line or two around the word, looking for clues." )
            , ( toString BreakDown, "Look for parts of words or whole words in the unknown word." )
            , ( toString Substitution, "Imagine the word isn't there and try another word or words in its place." )
            ]

        drwrBtn s evt =
            button [ class "btn btn-sm btn-default", onClick (ToggleDrawer evt) ] [ text s ]
    in
        Html.form []
            [ div [ class "form-group" ]
                [ answerField "connect" [ drwrBtn "?" Connect, text " Connect this story with yourself or something you know about." ]
                , answerField "question" [ drwrBtn "?" Question, text " Think of a question the story makes you want to ask and type it here." ]
                , answerField "summarise" [ drwrBtn "?" Summarise, text " Write one sentence that captures the main idea." ]
                , answerField "clarify" [ drwrBtn "?" Clarify, text " Work through the clarify methods then type what you think this word means: ", em [ class "clarify-word" ] [ text (.clarifyWord m.story) ] ]
                , div []
                    [ label [] [ text "Which clarify method worked best for you?" ]
                    , Input.selectInput clarifyMethodOptions (Form.getFieldAsString "clarifyMethod" m.form) [ class "form-control" ]
                        |> Html.map FormMsg
                    ]
                , submitButton "Submit your answers" |> Html.map FormMsg
                ]
            ]
