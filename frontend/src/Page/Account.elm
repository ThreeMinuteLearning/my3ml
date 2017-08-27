module Page.Account exposing (Model, Msg, init, update, view)

import Api
import Data.Session as Session exposing (Session, authorization, findStoryById)
import Data.Settings exposing (Settings, defaultSettings, encode, toStyle, fontOptions)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Task exposing (Task)
import Util exposing ((=>), defaultHttpErrorMsg)
import Views.Answers as Answers


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
            origSession.user
                |> Maybe.map .settings
                |> Maybe.withDefault defaultSettings

        zipWithStory session a =
            Maybe.map ((,) a) (findStoryById session.cache a.storyId)

        mkModel sesh =
            sesh.cache.answers
                |> Dict.values
                |> List.filterMap (zipWithStory sesh)
                |> Model settings
                |> \model -> ( model, sesh )
    in
        Session.loadUserAnswers origSession
            |> Task.andThen (\newSession -> Session.loadStories newSession)
            |> Task.map mkModel
            |> Task.mapError handleLoadError


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), Session )
update session msg ({ settings } as model) =
    case msg of
        SetBackground bg ->
            { model | settings = { settings | background = bg } }
                => Cmd.none
                => session

        SetColour c ->
            { model | settings = { settings | colour = c } }
                => Cmd.none
                => session

        SetFont f ->
            { model | settings = { settings | font = f } }
                => Cmd.none
                => session

        SetFontSize sz ->
            { model | settings = { settings | size = sz } }
                => Cmd.none
                => session

        SaveSettings ->
            model
                => (Api.postAccountSettings (authorization session) (encode model.settings)
                        |> Http.send SaveSettingsResponse
                   )
                => session

        SaveSettingsResponse (Ok _) ->
            model => Cmd.none => updateSessionSettings session model.settings

        SaveSettingsResponse (Err _) ->
            model => Cmd.none => session


updateSessionSettings : Session -> Settings -> Session
updateSessionSettings session newSettings =
    session.user
        |> Maybe.map (\u -> { u | settings = newSettings })
        |> Maybe.map (\u -> { session | user = Just u })
        |> Maybe.withDefault session


view : Model -> Html Msg
view { settings, answers } =
    div [ class "page container" ]
        [ viewSettings settings
        , Answers.viewWithStories answers
        ]


viewSettings : Settings -> Html Msg
viewSettings settings =
    div []
        [ h3 [] [ text "Story display settings" ]
        , div [ toStyle settings ]
            [ p [] [ text "You may wish to change the way your stories are displayed." ]
            , p [] [ text "Use the buttons below to make changes and then save the settings." ]
            , p [] [ text "You can change the background and text colour." ]
            , p [] [ text "Also the type of font and the size of the text." ]
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
                , ( "16px", "Medium" )
                , ( "20px", "Large" )
                , ( "23px", "Huge" )
                ]
            )
