module Page.Account exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Settings exposing (Settings, defaultSettings, toStyle, fontOptions)
import Exts.Html.Bootstrap as Bootstrap exposing (row)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
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
    | SetFont String
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

        SetFont f ->
            { model | settings = { settings | font = f } } => Cmd.none

        SetFontSize sz ->
            { model | settings = { settings | size = sz } } => Cmd.none

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
        , Html.form [ class "form-inline", onSubmit SaveSettings ]
            [ div [ class "form-group" ]
                [ label [ for "bg" ] [ text "Background colour:" ]
                , input [ type_ "color", id "bg", value settings.background, onInput SetBackground ] []
                ]
            , div [ class "form-group" ]
                [ label [ for "fg" ] [ text "Text colour:" ]
                , input [ type_ "color", id "fg", value settings.colour, onInput SetColour ] []
                ]
            , div [ class "form-group" ]
                [ label [ for "font" ] [ text "Font:" ]
                , fontSelect settings.font
                ]
            , div [ class "form-group" ]
                [ label [ for "sz" ] [ text "Font size:" ]
                , fontSizeSelect settings.size
                ]
            , button [ type_ "submit", class "btn btn-default" ] [ text "Save settings" ]
            ]
        ]


fontSelect : String -> Html Msg
fontSelect current =
    let
        mkOption ( font, fontFamily ) =
            option [ selected (current == fontFamily), value fontFamily ] [ text font ]
    in
        select [ id "font", class "form-control", onInput SetFont ]
            (List.map mkOption fontOptions)


fontSizeSelect : String -> Html Msg
fontSizeSelect current =
    let
        mkOption ( sz, nm ) =
            option [ selected (current == sz), value sz ] [ text nm ]
    in
        select [ id "sz", class "form-control", onInput SetFontSize ]
            (List.map mkOption
                [ ( "14px", "Small" )
                , ( "16px", "Standard" )
                , ( "18px", "Large" )
                , ( "20px", "Larger" )
                , ( "22px", "Huge" )
                ]
            )
