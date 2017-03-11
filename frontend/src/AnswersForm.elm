module AnswersForm exposing (Model, init, update, view)

import Dict
import Form exposing (Form)
import Form.Input as Input
import Form.Validate as Validate exposing (Validation, field, nonEmpty, string)
import Html exposing (Html, button, div, text, label)
import Html.Attributes exposing (id, class, for, type_)
import Html.Events exposing (onClick)


type ClarifyMethod
    = ReadAround
    | BreakDown
    | Substitution


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
    Form CustomError Answers


init : Model
init =
    Form.initial [] answerFormValidation


update : Form.Msg -> Model -> Model
update msg form =
    Form.update answerFormValidation msg form


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


view : Form CustomError Answers -> Html Form.Msg
view form =
    let
        answerField nm lbl =
            Form.getFieldAsString nm form
                |> \fld ->
                    div [ class (errorClass fld.liveError) ]
                        [ label [ for (nm ++ "Input") ] [ text lbl ]
                        , Input.textArea fld [ class "form-control", id (nm ++ "Input") ]
                        ]

        clarifyMethodOptions =
            [ ( "", "Please choose one" )
            , ( toString ReadAround, "Read a line or two around the word, looking for clues." )
            , ( toString BreakDown, "Look for parts of words or whole words in the unknown word." )
            , ( toString Substitution, "Imagine the word isn't there and try another word or words in its place." )
            ]

        errorClass maybeError =
            Maybe.map (\_ -> "has-error") maybeError |> Maybe.withDefault ""
    in
        Html.form []
            [ div [ class "form-group" ]
                [ answerField "connect" "Connect this story with yourself or something you know about."
                , answerField "question" "Think of a question the story makes you want to ask and type it here."
                , answerField "summarise" "Write one sentence that captures the main idea."
                , answerField "clarify" "Work through the clarify methods, then type what you think the word means."
                , div []
                    [ label [] [ text "Which clarify method worked best for you?" ]
                    , Input.selectInput clarifyMethodOptions (Form.getFieldAsString "clarifyMethod" form) [ class "form-control" ]
                    ]
                , button [ class "btn btn-primary", type_ "submit", onClick Form.Submit ] [ text "Submit your answers" ]
                ]
            ]
