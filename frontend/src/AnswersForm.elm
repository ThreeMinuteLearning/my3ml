module AnswersForm exposing (Msg, Model, init, update, view)

import Api
import Bootstrap exposing (errorClass)
import Data.Session exposing (Session, authorization)
import Dict exposing (Dict)
import Drawer exposing (DrawerType)
import Exts.List exposing (firstMatch)
import Html exposing (..)
import Html.Attributes exposing (id, class, for, selected, value, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Util exposing ((=>))
import Validate exposing (Validator, ifBlank, ifNothing)
import Views.Form as Form


type ClarifyMethod
    = ReadAround
    | BreakDown
    | Substitution


type Msg
    = ToggleDrawer DrawerType
    | SubmitForm
    | SetConnection String
    | SetQuestion String
    | SetSummary String
    | SetClarification String
    | SetClarifyMethod String
    | SubmitAnswersResponse (Result Http.Error Api.Answer)


type alias Model =
    { story : Api.Story
    , showDrawer : Maybe DrawerType
    , errors : List Error
    , connection : String
    , question : String
    , summary : String
    , clarification : String
    , clarificationMethod : Maybe ClarifyMethod
    }


init : Api.Story -> Model
init s =
    { story = s
    , showDrawer = Nothing
    , errors = []
    , connection = ""
    , question = ""
    , summary = ""
    , clarification = ""
    , clarificationMethod = Nothing
    }


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Maybe Api.Answer )
update session msg model =
    case msg of
        SubmitForm ->
            case validate model of
                [] ->
                    { model | errors = [] }
                        => submitAnswers session model
                        => Nothing

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => Nothing

        SetConnection connection ->
            { model | connection = connection }
                => Cmd.none
                => Nothing

        SetQuestion question ->
            { model | question = question }
                => Cmd.none
                => Nothing

        SetSummary summary ->
            { model | summary = summary }
                => Cmd.none
                => Nothing

        SetClarification clarification ->
            { model | clarification = clarification }
                => Cmd.none
                => Nothing

        SetClarifyMethod cm ->
            { model | clarificationMethod = Dict.get cm clarificationOptions }
                => Cmd.none
                => Nothing

        ToggleDrawer d ->
            if model.showDrawer == Just d then
                { model | showDrawer = Nothing }
                    => Cmd.none
                    => Nothing
            else
                { model | showDrawer = Just d }
                    => Cmd.none
                    => Nothing

        SubmitAnswersResponse (Ok answer) ->
            model
                => Cmd.none
                => Just answer

        SubmitAnswersResponse (Err _) ->
            { model | errors = model.errors ++ [ (Form => "Server error while trying to submit answers") ] }
                => Cmd.none
                => Nothing


clarificationOptions : Dict String ClarifyMethod
clarificationOptions =
    Dict.fromList
        [ ( toString ReadAround, ReadAround )
        , ( toString BreakDown, BreakDown )
        , ( toString Substitution, Substitution )
        ]


submitAnswers : Session -> Model -> Cmd Msg
submitAnswers session model =
    let
        answer =
            Api.Answer "" (.id model.story) "" model.connection model.question model.summary model.clarification
    in
        Api.postSchoolAnswers (authorization session) answer
            |> Http.send SubmitAnswersResponse


type Field
    = Form
    | Connection
    | Question
    | Summary
    | Clarification
    | ClarificationMethod


type alias Error =
    ( Field, String )


fieldError : Field -> List Error -> Maybe Error
fieldError field errors =
    firstMatch (\e -> Tuple.first e == field) errors


validate : Model -> List Error
validate =
    Validate.all
        [ .connection >> ifBlank (Connection => "Please fill in your connection with the story")
        , .question >> ifBlank (Question => "Please enter a question about the story")
        , .summary >> ifBlank (Summary => "Please write your summary sentence for the story")
        , .clarification >> ifBlank (Clarification => "Please fill in the meaning of the word")
        , .clarificationMethod >> ifNothing (ClarificationMethod => "Please select the clarification method you used")
        ]


view : Model -> Html Msg
view m =
    div []
        [ Form.viewErrors m.errors
        , viewForm m
        , Drawer.view (.showDrawer m) ToggleDrawer
        ]


viewForm : Model -> Html Msg
viewForm model =
    let
        answerField field msg lbl =
            div [ class (errorClass (fieldError field model.errors)) ]
                [ label [ for (toString field ++ "Input") ] lbl
                , Form.textarea [ class "form-control", id (toString field ++ "Input"), onInput msg ] []
                ]

        mkOption ( v, txt ) =
            Html.option
                [ selected (toString model.clarificationMethod == v)
                , value v
                ]
                [ text txt ]

        clarifyMethodOptions =
            List.map mkOption
                [ ( "", "Please choose one" )
                , ( toString ReadAround, "Read a line or two around the word, looking for clues." )
                , ( toString BreakDown, "Look for parts of words or whole words in the unknown word." )
                , ( toString Substitution, "Imagine the word isn't there and try another word or words in its place." )
                ]

        submitButton =
            Html.button [ class "btn btn-primary pull-xs-right" ] [ text "Submit your answers" ]

        drwrBtn s evt =
            button [ class "btn btn-sm btn-default", onClick (ToggleDrawer evt) ] [ text s ]
    in
        Html.form [ onSubmit SubmitForm ]
            [ div [ class "form-group" ]
                [ answerField Connection SetConnection [ drwrBtn "?" Drawer.Connect, text " Connect this story with yourself or something you know about." ]
                , answerField Question SetQuestion [ drwrBtn "?" Drawer.Question, text " Think of a question the story makes you want to ask and type it here." ]
                , answerField Summary SetSummary [ drwrBtn "?" Drawer.Summarise, text " Write one sentence that captures the main idea." ]
                , answerField Clarification SetClarification [ drwrBtn "?" Drawer.Clarify, text " Work through the clarify methods then type what you think this word means: ", em [ class "clarify-word" ] [ text (.clarifyWord model.story) ] ]
                , div [ class (errorClass (fieldError ClarificationMethod model.errors)) ]
                    [ label [ for "clarifyMethod" ] [ text "Which clarify method worked best for you?" ]
                    , Html.select [ id "clarifyMethod", class "form-control", onInput SetClarifyMethod ] clarifyMethodOptions
                    ]
                , submitButton
                ]
            ]
