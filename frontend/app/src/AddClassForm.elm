module AddClassForm exposing (Model, Msg, init, update, view)

import Api
import Bootstrap exposing (errorClass)
import Components
import Data.Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import List.Extra
import Util exposing (defaultHttpErrorMsg)
import Validate exposing (Validator, ifBlank, ifFalse, validate)
import Views.Form as Form


type alias Model =
    { errors : List Error
    , name : String
    , description : String
    }


init : Model
init =
    Model [] "" ""


type Msg
    = SubmitForm
    | SetName String
    | SetDescription String
    | AddClassResponse (Result Http.Error Api.Class)


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Maybe Api.Class )
update session msg model =
    case msg of
        SubmitForm ->
            case validate validator model of
                Err errors ->
                    ( ( { model | errors = errors }
                      , Cmd.none
                      )
                    , Nothing
                    )

                _ ->
                    ( ( { model | errors = [] }
                      , sendNewClassRequest session model
                      )
                    , Nothing
                    )

        SetName name ->
            ( ( { model | name = name }, Cmd.none ), Nothing )

        SetDescription d ->
            ( ( { model | description = d }, Cmd.none ), Nothing )

        AddClassResponse (Ok newClass) ->
            ( ( model, Cmd.none ), Just newClass )

        AddClassResponse (Err e) ->
            let
                errorMessage =
                    case e of
                        Http.BadStatus { status } ->
                            case status.code of
                                409 ->
                                    "A class with that name already exists"

                                _ ->
                                    defaultHttpErrorMsg e

                        _ ->
                            defaultHttpErrorMsg e
            in
            ( ( { model | errors = [ ( Form, errorMessage ) ] }, Cmd.none ), Nothing )


sendNewClassRequest : Session -> Model -> Cmd Msg
sendNewClassRequest session model =
    Api.postSchoolClasses (authorization session) ( model.name, model.description )
        |> Http.send AddClassResponse


type Field
    = Form
    | Name
    | Description


type alias Error =
    ( Field, String )


fieldError : Field -> List Error -> Maybe Error
fieldError field errors =
    List.Extra.find (\e -> Tuple.first e == field) errors


validator : Validator Error Model
validator =
    Validate.all
        [ ifFalse (validName << .name) ( Name, "You must enter a valid name for the class" )
        , ifBlank .description ( Description, "A description for the class is required" )
        ]


validName : String -> Bool
validName name =
    String.length (String.trim name)
        |> (\l -> l > 1 && l < 50)


view : Model -> Html Msg
view model =
    div []
        [ Form.viewErrors model.errors
        , Html.form [ onSubmit SubmitForm ]
            [ Form.input
                [ class "w-full mb-2"
                , class (errorClass (fieldError Name model.errors))
                , onInput SetName
                , placeholder "Class name"
                ]
                []
            , Form.input
                [ class "w-full mb-2"
                , class (errorClass (fieldError Description model.errors))
                , onInput SetDescription
                , placeholder "Class description"
                ]
                []
            , Components.btn [ type_ "submit", tabindex 3 ] [ text "Create class" ]
            ]
        ]
