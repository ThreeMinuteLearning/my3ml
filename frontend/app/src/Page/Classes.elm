module Page.Classes exposing (Model, Msg, init, update, view)

import AddClassForm
import Api
import Bootstrap exposing (link)
import Cache exposing (Cache)
import Data.Session as Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Modal
import Page.Errored exposing (PageLoadError, pageLoadError)
import Route
import Table
import Task exposing (Task)
import Util exposing (defaultHttpErrorMsg, maybeView)
import Views.TeacherToolbar as TeacherToolbar


type alias Model =
    { tableState : Table.State
    , addClassForm : Maybe AddClassForm.Model
    }


type Msg
    = SetTableState Table.State
    | AddClassFormMsg AddClassForm.Msg
    | DismissAddClass
    | ShowAddClass


init : Session -> Task PageLoadError ( Model, Session )
init session =
    let
        handleLoadError e =
            pageLoadError e ("Unable to load classes. " ++ defaultHttpErrorMsg e ++ ".")

        createModel sesh =
            ( Model (Table.initialSort "Class Name") Nothing, sesh )
    in
    Session.loadClasses session
        |> Task.mapError handleLoadError
        |> Task.map createModel


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Session )
update session msg model =
    case msg of
        ShowAddClass ->
            ( ( { model | addClassForm = Just AddClassForm.init }, Cmd.none ), session )

        DismissAddClass ->
            ( ( { model | addClassForm = Nothing }, Cmd.none ), session )

        SetTableState state ->
            ( ( { model | tableState = state }, Cmd.none ), session )

        AddClassFormMsg subMsg ->
            case Maybe.map (AddClassForm.update session subMsg) model.addClassForm of
                Nothing ->
                    ( ( model, Cmd.none ), session )

                Just ( ( subModel, subSubMsg ), Nothing ) ->
                    ( ( { model | addClassForm = Just subModel }, Cmd.map AddClassFormMsg subSubMsg ), session )

                Just ( _, Just newClass ) ->
                    let
                        cache =
                            Session.getCache session

                        newClasses =
                            newClass :: cache.classes

                        newSession =
                            Session.updateCache (\c -> { c | classes = newClasses }) session
                    in
                    ( ( { model | addClassForm = Nothing }, Cmd.none ), newSession )


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title = "Classes"
    , content =
        div [ class "flex flex-col" ]
            [ div [ class "mb-16" ] [ TeacherToolbar.view session Route.Classes subtools ]
            , viewTable (Session.getCache session) model
            , maybeView addClassesDialog model.addClassForm
            ]
    }


viewTable : Cache -> Model -> Html Msg
viewTable cache model =
    Table.view classesTableConfig model.tableState cache.classes


classesTableConfig : Table.Config Api.Class Msg
classesTableConfig =
    Table.customConfig
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , Table.stringColumn "Description" (Maybe.withDefault "" << .description)
            , Table.intColumn "Number of Students" (List.length << .students)
            ]
        , customizations = Bootstrap.tableCustomizations
        }


nameColumn : Table.Column Api.Class Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Class name"
        , viewData = viewClassLink
        , sorter = Table.increasingOrDecreasingBy .name
        }


viewClassLink : Api.Class -> Table.HtmlDetails Msg
viewClassLink class =
    Table.HtmlDetails []
        [ link (Route.href (Route.Teacher (Route.Class class.id))) class.name
        ]


subtools : List (Html Msg)
subtools =
    [ Bootstrap.btn "add-class-button" ShowAddClass "Add Class" ]


addClassesDialog : AddClassForm.Model -> Html Msg
addClassesDialog form =
    Modal.view "Add Class"
        DismissAddClass
        (div [ class "w-full max-w-xl p-4 flex flex-col" ]
            [ p [ class "mb-2 text-lg" ] [ text "Please enter a name and a description for the new class" ]
            , Html.map AddClassFormMsg (AddClassForm.view form)
            ]
        )
