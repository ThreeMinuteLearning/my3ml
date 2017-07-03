module Page.Class exposing (Model, Msg, ExternalMsg(..), init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization)
import Dialog
import Exts.Html.Bootstrap exposing (row)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Util exposing ((=>), dialog, viewIf)
import Views.Page as Page


type alias Model =
    { class : Api.Class
    , showConfirmDelete : Bool
    }


type Msg
    = Delete
    | Edit
    | ConfirmDelete
    | DismissDialog
    | DeleteResponse (Result Http.Error Api.NoContent)


type ExternalMsg
    = NoOp
    | Deleted Session


init : Session -> String -> Task PageLoadError ( Model, Session )
init session_ slug =
    let
        handleLoadError _ =
            pageLoadError Page.Other "Unable to load data for page."

        loadClass =
            Api.getSchoolClassesByClassId (authorization session_) slug
                |> Http.toTask

        mkModel newSession class =
            ( Model class False, newSession )
    in
        Task.map2 mkModel (Session.loadStudents session_) loadClass
            |> Task.mapError handleLoadError


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        Delete ->
            { model | showConfirmDelete = True } => Cmd.none => NoOp

        DismissDialog ->
            { model | showConfirmDelete = False } => Cmd.none => NoOp

        ConfirmDelete ->
            model
                => (Api.deleteSchoolClassesByClassId (authorization session) (.id model.class)
                        |> Http.send DeleteResponse
                   )
                => NoOp

        DeleteResponse (Ok _) ->
            { model | showConfirmDelete = False }
                => Cmd.none
                => Deleted (deleteFromCache model.class session)

        DeleteResponse (Err _) ->
            model => Cmd.none => NoOp

        Edit ->
            model => Cmd.none => NoOp


deleteFromCache : Api.Class -> Session -> Session
deleteFromCache _ session =
    let
        cache =
            session.cache
    in
        -- Just clear the cache completely for now
        { session | cache = { cache | classes = [] } }


view : Session -> Model -> Html Msg
view session model =
    div [ class "container page" ]
        [ h3 [] [ text (.name model.class) ]
        , h4 [] [ text (Maybe.withDefault "" (.description model.class)) ]
        , viewToolbar model.class
        , Dialog.view
            (if model.showConfirmDelete then
                Just (confirmDeleteDialog (userIsOwner session.user model.class))
             else
                Nothing
            )
        ]


userIsOwner : Maybe Session.User -> Api.Class -> Bool
userIsOwner user class =
    case user of
        Nothing ->
            False

        Just u ->
            u.sub == class.createdBy


viewToolbar : Api.Class -> Html Msg
viewToolbar student =
    let
        inputGroupBtn msg txt =
            button [ class "btn btn-default", onClick msg, type_ "button" ] [ text txt ]
    in
        row
            [ div [ class "col-lg-6" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-btn" ]
                        [ inputGroupBtn Edit "Edit"
                        , inputGroupBtn Delete "Delete"
                        ]
                    ]
                ]
            ]


confirmDeleteDialog : Bool -> Dialog.Config Msg
confirmDeleteDialog isOwner =
    dialog DismissDialog
        Nothing
        (div []
            [ p [] [ text "Are you sure you want to delete this class? Only the class information will be removed (none of the student accounts will be affected)." ]
            , viewIf (not isOwner) <|
                p [] [ text "WARNING: Another teacher created this class. Perhaps you shouldn't delete it (they might be annoyed!)?" ]
            , button [ class "btn btn-warn", onClick ConfirmDelete ]
                [ text "Delete class"
                ]
            ]
        )
