module AnswersForm exposing (Msg, Model, init, update, view)

import Api
import Bootstrap exposing (errorClass)
import Data.Session exposing (Session, authorization)
import Dict exposing (Dict)
import Drawer exposing (DrawerType)
import Exts.List exposing (firstMatch)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onSubmit, targetValue)
import Http
import Json.Decode as Json
import Regex
import Util exposing ((=>))
import Validate exposing (Validator, ifBlank, ifNothing, ifInvalid)
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
            case validate (trim model) of
                [] ->
                    { model | errors = [] }
                        => submitAnswers session (trim model)
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


trim : Model -> Model
trim m =
    { m | connection = String.trim m.connection, question = String.trim m.question, summary = String.trim m.summary, clarification = String.trim m.clarification }


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
            Api.Answer (.id model.story) "" model.connection model.question model.summary model.clarification
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
        [ .connection
            >> Validate.all
                [ ifBlank ( Connection, "Please fill in your connection with the story" )
                , ifNotSentence ( Connection, "Please write a sentence for your connection with the story" )
                , ifTooLong Connection
                ]
        , .question
            >> Validate.all
                [ ifBlank ( Question, "Please enter a question about the story" )
                , ifNotSentence ( Question, "Please write a sentence for your question" )
                , ifTooLong Question
                ]
        , .summary
            >> Validate.all
                [ ifBlank ( Summary, "Please write your summary sentence for the story" )
                , ifNotSentence ( Summary, "Please the summary as a sentence" )
                , ifTooLong Summary
                ]
        , .clarification
            >> Validate.all
                [ ifBlank ( Clarification, "Please fill in the meaning of the word" )
                , ifNotSentence ( Clarification, "Please write a sentence for the meaning of the word" )
                ]
        , .clarificationMethod >> ifNothing ( ClarificationMethod, "Please select the clarification method you used" )
        , naughtyWordsCheck
        ]


ifTooLong : Field -> Validator Error String
ifTooLong f =
    ifInvalid (\s -> String.length s > 250) ( f, "The answer can't be longer than 250 characters" )


ifNotSentence : Error -> Validator Error String
ifNotSentence =
    let
        notSentence =
            Regex.regex "^\\s*\\S+\\s*$"
    in
        ifInvalid (Regex.contains notSentence)


naughtyWordsCheck : Model -> List Error
naughtyWordsCheck m =
    let
        naughtyWord =
            Regex.regex "fuc*k+|bastard|bugger\\b|\\bshite*\\b"
                |> Regex.caseInsensitive

        hasNaughtyWord =
            List.map (Regex.contains naughtyWord) [ m.connection, m.question, m.summary, m.clarification ]
                |> List.foldl (||) False
    in
        if hasNaughtyWord then
            [ ( Form, "Sorry, that's not allowed" ) ]
        else
            []


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
        answerField field msg lbl tx =
            div [ class (errorClass (fieldError field model.errors)) ]
                [ label [ for (toString field ++ "Input") ] lbl
                , Form.textarea [ class "form-control", id (toString field ++ "Input"), onInput msg, tabindex tx ] []
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

        onSelect msg =
            on "change" (Json.map msg targetValue)

        submitButton =
            Html.button [ class "btn btn-primary pull-xs-right", tabindex 6 ] [ text "Submit your answers" ]

        drwrBtn s evt =
            button [ class "btn btn-sm btn-default", tabindex -1, onClick (ToggleDrawer evt), type_ "button" ] [ text s ]
    in
        Html.form [ onSubmit SubmitForm ]
            [ div [ class "form-group" ]
                [ answerField Connection SetConnection [ drwrBtn "?" Drawer.Connect, text " Connect this story with yourself or something you know about." ] 1
                , answerField Question SetQuestion [ drwrBtn "?" Drawer.Question, text " Think of a question the story makes you want to ask and type it here." ] 2
                , answerField Summary SetSummary [ drwrBtn "?" Drawer.Summarise, text " Write one sentence that captures the main idea." ] 3
                , answerField Clarification SetClarification [ drwrBtn "?" Drawer.Clarify, text " Work through the clarify methods then type what you think this word means: ", em [ class "clarify-word" ] [ text (.clarifyWord model.story) ] ] 4
                , div [ class (errorClass (fieldError ClarificationMethod model.errors)) ]
                    [ label [ for "clarifyMethod" ] [ text "Which clarify method worked best for you?" ]
                    , Html.select [ id "clarifyMethod", class "form-control", onSelect SetClarifyMethod, tabindex 5 ] clarifyMethodOptions
                    ]
                , submitButton
                ]
            ]
