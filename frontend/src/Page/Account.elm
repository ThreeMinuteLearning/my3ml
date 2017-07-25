module Page.Account exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Settings exposing (Settings, defaultSettings, toStyle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Value)
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Task exposing (Task)
import Util exposing ((=>))


type alias Model =
    { settings : Settings
    }


type Msg
    = SetColour String
    | SetBackground String
    | SetFontSize String
    | SaveSettings
    | SaveSettingsResponse (Result Http.Error Decode.Value)


init : Session -> Task PageLoadError Model
init session =
    let
        settings =
            session.user
                |> Maybe.map .settings
                |> Maybe.withDefault defaultSettings
    in
        Task.succeed (Model settings)


update : Msg -> Model -> ( Model, Cmd msg )
update msg ({ settings } as model) =
    case msg of
        SetBackground bg ->
            { model | settings = { settings | background = bg } } => Cmd.none

        SetColour c ->
            { model | settings = { settings | colour = c } } => Cmd.none

        _ ->
            model => Cmd.none


view : Model -> Html Msg
view { settings } =
    div [ class "page container" ]
        [ h3 [] [ text "Story display settings" ]
        , div [ toStyle settings ]
            [ p [] [ text "Summertime" ]
            , p [] [ text "And the living is easy" ]
            , p [] [ text "Fish are jumping" ]
            , p [] [ text "And the cotton is high" ]
            ]
        , label [ for "background" ] [ text "Background colour" ]
        , div [ class "input-group" ]
            [ input [ type_ "color", id "background", value settings.background, onInput SetBackground ] []
            ]
        , label [ for "colour" ] [ text "Text colour" ]
        , div [ class "input-group" ]
            [ input [ type_ "color", id "colour", value settings.colour, onInput SetColour ] []
            ]
        ]
