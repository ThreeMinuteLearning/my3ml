module AnswersForm exposing (Answers, DrawerType(..), Msg(..), Model, init, update, view)

import Api exposing (Story)
import Bootstrap exposing (errorClass, submitButton)
import Dict
import Form exposing (Form)
import Form.Input as Input
import Form.Validate as Validate exposing (Validation, field, nonEmpty, string, succeed)
import Html exposing (Html, button, div, text, label)
import Html.Attributes exposing (id, class, for, type_)
import Html.Events exposing (onClick)


type ClarifyMethod
    = ReadAround
    | BreakDown
    | Substitution


type DrawerType
    = Connect
    | Question
    | Summarise
    | Clarify


type Msg
    = ToggleDrawer DrawerType
    | FormMsg Form.Msg


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
    { story : Story
    , showDrawer : Maybe DrawerType
    , form : Form CustomError Answers
    }


init : Story -> Model
init s =
    { story = s, showDrawer = Nothing, form = Form.initial [] answerFormValidation }


update : Msg -> Model -> Model
update msg m =
    case msg of
        ToggleDrawer d ->
            if m.showDrawer == Just d then
                { m | showDrawer = Nothing }
            else
                { m | showDrawer = Just d }

        FormMsg fMsg ->
            { m | form = Form.update answerFormValidation fMsg m.form }


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
                , answerField "clarify" [ drwrBtn "?" Clarify, text " Work through the clarify methods then type what you think the word means." ]
                , div []
                    [ label [] [ text "Which clarify method worked best for you?" ]
                    , Input.selectInput clarifyMethodOptions (Form.getFieldAsString "clarifyMethod" m.form) [ class "form-control" ]
                        |> Html.map FormMsg
                    ]
                , submitButton "Submit your answers" |> Html.map FormMsg
                ]
            ]
