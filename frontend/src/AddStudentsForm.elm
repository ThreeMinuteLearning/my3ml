module AddStudentsForm exposing (Model, init, update, view)

import Bootstrap exposing (errorClass, submitButton)
import Exts.Html.Bootstrap exposing (formGroup)
import Form exposing (Form)
import Form.Input as Input
import Form.Validate as Validate exposing (Validation, andThen, emptyString, field, nonEmpty, oneOf, minLength, maxLength, sequence, string, succeed)
import Html exposing (Html, button, div, p, text, label)
import Html.Attributes exposing (class, id)


type alias Model =
    Form () (List String)


init : Model
init =
    Form.initial [] validation


update : Form.Msg -> Model -> Model
update msg form =
    Form.update validation msg form


validation : Validation () (List String)
validation =
    let
        validateFirstItem =
            field "nameInput1" validName

        validateRemaining =
            List.tail formFieldNames
                |> Maybe.withDefault []
                |> List.map (flip field (oneOf [ emptyString, validName ]))
    in
        sequence (validateFirstItem :: validateRemaining)


validName : Validation () String
validName =
    string |> andThen (minLength 3) |> andThen (maxLength 50)


view : Model -> Html Form.Msg
view form =
    let
        input nm =
            Form.getFieldAsString nm form
                |> \fld ->
                    div [ class (errorClass fld.liveError) ]
                        [ Input.textInput fld [ class "form-control", id (nm ++ "Input") ]
                        ]

        fields =
            List.map input formFieldNames

        errorMsg =
            if List.isEmpty (Form.getErrors form) || not (Form.isSubmitted form) then
                ""
            else
                "You must enter at least one valid name."

        errors =
            p [ class "help-block" ]
                [ text errorMsg ]
    in
        Html.form []
            [ formGroup <| List.append fields <| errors :: [ submitButton "Create Accounts" ]
            ]


formFieldNames =
    List.range 1 10 |> List.map (toString >> (++) "Input" >> (++) "name")
