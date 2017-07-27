module Page.Editor exposing (Model, Msg, init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Data.Settings exposing (defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Ports
import Task exposing (Task)
import Util exposing ((=>))
import Views.Story as Story


type alias Model =
    { story : Api.Story
    , picWidth : Int
    }


type Msg
    = ContentInput String
    | GetImgWidth String
    | ImageWidth Float
    | Save
    | SaveResponse (Result Http.Error Api.Story)


init : Session -> String -> Task PageLoadError ( Model, Session )
init originalSession slug =
    let
        handleLoadError e =
            pageLoadError e "Story is currently unavailable."

        lookupStoryAndCreateModel session =
            case findStoryById session.cache slug of
                Just story ->
                    Task.succeed ( Model story 0, session )

                Nothing ->
                    Task.fail (PageLoadError "Sorry. That story couldn't be found.")
    in
        Session.loadDictionary originalSession
            |> Task.andThen (\newSession -> Session.loadStories newSession)
            |> Task.mapError handleLoadError
            |> Task.andThen lookupStoryAndCreateModel


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg ({ story } as model) =
    case msg of
        GetImgWidth s ->
            model => Ports.getImgWidth s

        ImageWidth w ->
            { model | picWidth = round w } => Cmd.none

        ContentInput newContent ->
            { model | story = { story | content = newContent } }
                => Cmd.none

        Save ->
            model
                => (Api.postStoriesByStoryId (authorization session) model.story.id story
                        |> Http.send SaveResponse
                   )

        SaveResponse (Ok story) ->
            { model | story = story } => Cmd.none

        SaveResponse (Err _) ->
            ( model, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ Ports.imgWidth ImageWidth ]


view : Model -> Html Msg
view model =
    div [ id "editor", class "container page" ]
        [ div [ class "row panes" ]
            [ div [ class "col-xs-1" ] []
            , div [ class "col-md-5 contentinput" ]
                [ textarea
                    [ value model.story.content
                    , class "form-control"
                    , onInput ContentInput
                    , rows 30
                    ]
                    []
                ]
            , div [ class "col-md-5" ]
                [ Story.view defaultSettings model.story model.picWidth GetImgWidth ]
            ]
        , div [ class "row" ]
            [ div [ class "col-xs-1" ] []
            , div [ class "col-md-5" ]
                [ button [ class "btn btn-default", onClick Save ] [ text "Save Changes" ]
                ]
            ]
        ]
