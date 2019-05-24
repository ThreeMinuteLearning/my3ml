module Page.Account exposing (Model, Msg, init, update, view)

import Api
import Components
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Data.Settings exposing (Settings, defaultSettings, encode, fontOptions, toStyle)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Task exposing (Task)
import Tuple exposing (pair)
import Util exposing (defaultHttpErrorMsg, viewUnless)
import Views.Answers as Answers
import Views.Form as Form


type alias Model =
    { settings : Settings
    , answers : List ( Api.Answer, Api.Story )
    }


type Msg
    = SetColour String
    | SetBackground String
    | SetFont String
    | SetFontSize String
    | SaveSettings
    | SaveSettingsResponse (Result Http.Error Api.NoContent)


init : Session -> Task PageLoadError ( Model, Session )
init origSession =
    let
        handleLoadError e =
            pageLoadError e (defaultHttpErrorMsg e)

        settings =
            Session.getSettings origSession
                |> Maybe.withDefault defaultSettings

        zipWithStory cache a =
            Maybe.map (pair a) (findStoryById cache a.storyId)

        mkModel cache =
            cache.answers
                |> Dict.values
                |> List.filterMap (zipWithStory cache)
                |> Model settings
    in
    Session.loadUserAnswers origSession
        |> Task.andThen Session.loadStories
        |> Task.map (\newSession -> ( mkModel (Session.getCache newSession), newSession ))
        |> Task.mapError handleLoadError


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Session )
update session msg ({ settings } as model) =
    case msg of
        SetBackground bg ->
            ( ( { model | settings = { settings | background = bg } }, Cmd.none ), session )

        SetColour c ->
            ( ( { model | settings = { settings | colour = c } }, Cmd.none ), session )

        SetFont f ->
            ( ( { model | settings = { settings | font = f } }, Cmd.none ), session )

        SetFontSize sz ->
            ( ( { model | settings = { settings | size = sz } }, Cmd.none ), session )

        SaveSettings ->
            ( ( model
              , Api.postAccountSettings (authorization session) (encode model.settings)
                    |> Http.send SaveSettingsResponse
              )
            , session
            )

        SaveSettingsResponse (Ok _) ->
            ( ( model, Cmd.none ), Session.updateSettings session model.settings )

        SaveSettingsResponse (Err _) ->
            ( ( model, Cmd.none ), session )


view : Model -> { title : String, content : Html Msg }
view { settings, answers } =
    { title = "My 3ml"
    , content =
        div []
            [ viewSettings settings
            , viewUnless (List.isEmpty answers) (h1 [ class "text-2xl font-light mb-2" ] [ text "My story answers" ])
            , Answers.viewWithStories answers
            ]
    }


viewSettings : Settings -> Html Msg
viewSettings settings =
    div [ class "mb-4" ]
        [ h1 [ class "text-2xl font-light mb-2" ] [ text "Story display settings" ]
        , div (class "p-4 border rounded mb-2" :: toStyle settings)
            [ p [ class "mb-2" ] [ text "You may wish to change the way your stories are displayed." ]
            , p [ class "mb-2" ] [ text "Use the controls below to make changes and then save the settings." ]
            , p [ class "mb-2" ] [ text "You can change the background and text colour." ]
            , p [ class "mb-2" ] [ text "You can also choose the font and the size of the text." ]
            ]
        , Html.form [ class "flex flex-col", onSubmit SaveSettings ]
            [ div [ class "flex mb-2" ]
                [ div [ class "flex items-center mr-4" ]
                    [ label [ for "bg", class "mr-2" ] [ text "Background colour:" ]
                    , input [ type_ "color", id "bg", value settings.background, onInput SetBackground ] []
                    ]
                , div [ class "flex items-center" ]
                    [ label [ for "fg", class "mr-2" ] [ text "Text colour:" ]
                    , input [ type_ "color", id "fg", value settings.colour, onInput SetColour ] []
                    ]
                ]
            , div [ class "flex mb-2" ]
                [ div [ class "flex items-center mr-4" ]
                    [ label [ for "font", class "mr-2" ] [ text "Font:" ]
                    , fontSelect settings.font
                    ]
                , div [ class "flex items-center" ]
                    [ label [ for "sz", class "mr-2" ] [ text "Font size:" ]
                    , fontSizeSelect settings.size
                    ]
                ]
            , Components.btn [ type_ "submit", class "max-w-xs" ] [ text "Save settings" ]
            ]
        ]


fontSelect : String -> Html Msg
fontSelect current =
    let
        mkOption ( font, fontFamily ) =
            option [ selected (current == fontFamily), value fontFamily ] [ text font ]
    in
    Form.select [ id "font", onInput SetFont ]
        (List.map mkOption fontOptions)


fontSizeSelect : String -> Html Msg
fontSizeSelect current =
    let
        mkOption ( sz, nm ) =
            option [ selected (current == sz), value sz ] [ text nm ]
    in
    Form.select [ id "sz", onInput SetFontSize ]
        (List.map mkOption
            [ ( "0.875rem", "Small" )
            , ( "1rem", "Normal" )
            , ( "1.25rem", "Large" )
            , ( "1.875rem", "Huge" )
            , ( "2.25rem", "Gigantic" )
            ]
        )
