module AnswersForm exposing (Model, Msg, init, update, view)

import Api
import Bootstrap exposing (errorClass)
import Data.Session exposing (Session, authorization)
import Dict exposing (Dict)
import Drawer exposing (DrawerType)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onSubmit, targetValue)
import Http
import Json.Decode as Json
import List.Extra
import Regex
import Util exposing (viewIf)
import Validate exposing (Validator, fromErrors, ifBlank, ifNothing, ifTrue, validate)
import Views.Form as Form


type ClarifyMethod
    = ReadAround
    | BreakDown
    | Substitution


toString : ClarifyMethod -> String
toString c =
    case c of
        ReadAround ->
            "ReadAround"

        BreakDown ->
            "BreakDown"

        Substitution ->
            " Substitution"


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
    , formSubmitted : Bool
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
    , formSubmitted = False
    }


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Maybe Api.Answer )
update session msg model =
    case msg of
        SubmitForm ->
            case validate validator (trim model) of
                Err errors ->
                    ( ( { model | errors = errors }, Cmd.none ), Nothing )

                _ ->
                    ( ( { model | errors = [], formSubmitted = True }, submitAnswers session (trim model) ), Nothing )

        SetConnection connection ->
            ( ( { model | connection = connection }, Cmd.none ), Nothing )

        SetQuestion question ->
            ( ( { model | question = question }, Cmd.none ), Nothing )

        SetSummary summary ->
            ( ( { model | summary = summary }, Cmd.none ), Nothing )

        SetClarification clarification ->
            ( ( { model | clarification = clarification }, Cmd.none ), Nothing )

        SetClarifyMethod cm ->
            ( ( { model | clarificationMethod = Dict.get cm clarificationOptions }, Cmd.none ), Nothing )

        ToggleDrawer d ->
            if model.showDrawer == Just d then
                ( ( { model | showDrawer = Nothing }, Cmd.none ), Nothing )

            else
                ( ( { model | showDrawer = Just d }, Cmd.none ), Nothing )

        SubmitAnswersResponse (Ok answer) ->
            ( ( model, Cmd.none ), Just answer )

        SubmitAnswersResponse (Err _) ->
            ( ( { model | errors = model.errors ++ [ ( Form, "Server error while trying to submit answers" ) ], formSubmitted = False }, Cmd.none ), Nothing )


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


fieldToString : Field -> String
fieldToString f =
    case f of
        Form ->
            "Form"

        Connection ->
            "Connection"

        Question ->
            "Question"

        Summary ->
            "Summary"

        Clarification ->
            "Clarification"

        ClarificationMethod ->
            "ClarificationMethod"


type alias Error =
    ( Field, String )


fieldError : Field -> List Error -> Maybe Error
fieldError field errors =
    List.Extra.find (\e -> Tuple.first e == field) errors


validator : Validator Error Model
validator =
    Validate.all
        [ ifBlank .connection ( Connection, "Please fill in your connection with the story" )
        , ifNotSentence .connection ( Connection, "Please write a sentence for your connection with the story" )
        , ifTooLong .connection Connection
        , ifBlank .question ( Question, "Please enter a question about the story" )
        , ifNotSentence .question ( Question, "Please write a sentence for your question" )
        , ifTooLong .question Question
        , ifBlank .summary ( Summary, "Please write your summary sentence for the story" )
        , ifNotSentence .summary ( Summary, "Please the summary as a sentence" )
        , ifTooLong .summary Summary
        , ifBlank .clarification ( Clarification, "Please fill in the meaning of the word" )
        , ifNotSentence .clarification ( Clarification, "Please write a sentence for the meaning of the word" )
        , ifNothing .clarificationMethod ( ClarificationMethod, "Please select the clarification method you used" )
        , fromErrors naughtyWordsCheck
        ]


ifTooLong : (Model -> String) -> Field -> Validator Error Model
ifTooLong f field =
    ifTrue (\m -> String.length (f m) > 250) ( field, "The answer can't be longer than 250 characters" )


ifNotSentence : (Model -> String) -> Error -> Validator Error Model
ifNotSentence f =
    let
        notSentence =
            Regex.fromString "^\\s*\\S+\\s*$"
                |> Maybe.withDefault Regex.never
    in
    ifTrue (\m -> Regex.contains notSentence (f m))


naughtyWordsCheck : Model -> List Error
naughtyWordsCheck m =
    let
        naughtyWord =
            Regex.fromStringWith { caseInsensitive = True, multiline = False } "fuc*k+|bastard|bugger\\b|\\bshite*\\b"
                |> Maybe.withDefault Regex.never

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
        , viewIf m.formSubmitted (p [] [ text "Submitting answers ..." ])
        , viewForm m
        , Drawer.view m.showDrawer ToggleDrawer
        ]


viewForm : Model -> Html Msg
viewForm model =
    let
        answerField field msg lbl tx =
            div [ class (errorClass (fieldError field model.errors)) ]
                [ label [ for (fieldToString field ++ "Input") ] lbl
                , Form.textarea [ class "form-control", id (fieldToString field ++ "Input"), onInput msg, tabindex tx, disabled model.formSubmitted ] []
                ]

        mkOption ( v, txt ) =
            Html.option
                [ selected (Maybe.map toString model.clarificationMethod == Just v)
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
            Html.button [ class "btn btn-primary pull-xs-right", tabindex 6, disabled model.formSubmitted ] [ text "Submit your answers" ]

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
