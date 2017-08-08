module Page.Editor exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Data.Settings exposing (defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Multiselect
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Ports
import Set
import Task exposing (Task)
import Tuple exposing (second)
import Util exposing ((=>))
import Views.Story as Story


type alias Model =
    { story : Api.Story
    , picWidth : Int
    , sqaTags : List String
    , tagsMultiselect : Multiselect.Model
    }


type Msg
    = ContentInput String
    | GetImgWidth String
    | ImageWidth Float
    | Save
    | SaveResponse (Result Http.Error Api.Story)
    | MSMsg Multiselect.Msg


init : Session -> Int -> Task PageLoadError ( Model, Session )
init originalSession slug =
    let
        handleLoadError e =
            pageLoadError e "Story is currently unavailable."

        lookupStoryAndCreateModel session =
            case findStoryById session.cache slug of
                Just story ->
                    Task.succeed
                        ( Model story
                            0
                            (makeSqaTags session.cache.stories)
                            (initMultiselect session.cache.stories story.tags)
                        , session
                        )

                Nothing ->
                    Task.fail (PageLoadError "Sorry. That story couldn't be found.")
    in
        Session.loadDictionary originalSession
            |> Task.andThen (\newSession -> Session.loadStories newSession)
            |> Task.mapError handleLoadError
            |> Task.andThen lookupStoryAndCreateModel


makeSqaTags : List Api.Story -> List String
makeSqaTags =
    List.map .qualification >> Set.fromList >> Set.toList


initMultiselect : List Api.Story -> List String -> Multiselect.Model
initMultiselect stories selection =
    let
        tags =
            stories
                |> List.concatMap .tags
                |> Set.fromList
                |> Set.toList
                |> \ts -> List.map2 (,) ts ts

        ms =
            Multiselect.initModel tags "Tags"
    in
        { ms | selected = List.map2 (,) selection selection }


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

        MSMsg sub ->
            let
                ( subModel, subCmd ) =
                    Multiselect.update sub model.tagsMultiselect

                selected =
                    List.map second (Multiselect.getSelectedValues subModel)

                newStory =
                    { story | tags = selected }
            in
                { model | tagsMultiselect = subModel, story = newStory } ! [ Cmd.map MSMsg subCmd ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.imgWidth ImageWidth
        , Sub.map MSMsg <| Multiselect.subscriptions model.tagsMultiselect
        ]


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
            , div [ class "col-md-10" ]
                [ Html.map MSMsg <| Multiselect.view model.tagsMultiselect
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-xs-1" ] []
            , div [ class "col-md-5" ]
                [ button [ class "btn btn-default", onClick Save ] [ text "Save Changes" ]
                ]
            ]
        ]
