module AddClassForm exposing (Model, init, update, view)

import Bootstrap exposing (errorClass, submitButton)
import Exts.Html.Bootstrap exposing (formGroup)
import Form exposing (Form)
import Form.Input as Input
import Form.Validate as Validate exposing (Validation, andThen, emptyString, field, nonEmpty, oneOf, minLength, maxLength, sequence, string, succeed)
import Html exposing (Html, button, div, p, text, label)
import Html.Attributes exposing (class, id, for)


type alias Model =
    Form () ( String, String )


init : Model
init =
    Form.initial [] validation


update : Form.Msg -> Model -> Model
update msg form =
    Form.update validation msg form


validation : Validation () ( String, String )
validation =
    Validate.map2 (,)
        (field "className" validName)
        (field "description" string |> Validate.andThen nonEmpty)


validName : Validation () String
validName =
    string |> andThen (minLength 2) |> andThen (maxLength 50)


view : Model -> Html Form.Msg
view form =
    let
        input nm lbl =
            Form.getFieldAsString nm form
                |> \fld ->
                    div [ class (errorClass fld.liveError) ]
                        [ label [ for nm ] [ text lbl ]
                        , Input.textInput fld [ class "form-control", id nm ]
                        ]

        errorMsg =
            if List.isEmpty (Form.getErrors form) || not (Form.isSubmitted form) then
                ""
            else
                "Please enter both a name and description for the class."

        errors =
            p [ class "help-block" ]
                [ text errorMsg ]
    in
        Html.form []
            [ formGroup <| input "className" "Class name" :: input "description" "Description" :: errors :: [ submitButton "Create Class" ]
            ]


formFieldNames =
    List.range 1 10 |> List.map (toString >> (++) "Input" >> (++) "name")
