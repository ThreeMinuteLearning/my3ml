module Main exposing (main)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Dashboard
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Value)
import Page.Login as Login
import Util exposing (defaultHttpErrorMsg)


type alias Model =
    { accessToken : Maybe String
    , ua : String
    , page : Page
    }


type Page
    = Login Login.Model
    | Stats (Maybe Dashboard.Stats)
    | Error String


type Msg
    = StatsLoaded (Result Http.Error Value)
    | LoginMsg (Login.Msg Api.Login)
    | SortSchoolsBy Dashboard.SortSchools


init : String -> ( Model, Cmd Msg )
init ua =
    ( Model Nothing ua (Login Login.initialModel), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( Login subModel, LoginMsg subMsg ) ->
            let
                loginRequest username password otp =
                    Api.postAuthenticate (Api.LoginRequest username password otp model.ua)

                ( ( pageModel, loginCmd ), maybeLoggedIn ) =
                    Login.update subMsg subModel loginRequest

                ( newPage, newCmd ) =
                    case maybeLoggedIn of
                        Nothing ->
                            ( Login pageModel, Cmd.map LoginMsg loginCmd )

                        Just u ->
                            ( Stats Nothing, loadStats (.token u) )
            in
            ( { model | accessToken = Maybe.map .token maybeLoggedIn, page = newPage }, newCmd )

        ( Stats s, SortSchoolsBy sortBy ) ->
            ( { model | page = Stats (Maybe.map (Dashboard.sortSchools sortBy) s) }, Cmd.none )

        ( Stats _, StatsLoaded (Ok json) ) ->
            case Decode.decodeValue Dashboard.decodeStats json of
                Ok stats ->
                    ( { model | page = Stats (Just stats) }, Dashboard.elmToVega (Dashboard.vegaSpec stats) )

                Err e ->
                    ( { model | page = Error (Decode.errorToString e) }, Cmd.none )

        ( Stats _, StatsLoaded (Err err) ) ->
            ( { model | page = Error (defaultHttpErrorMsg err) }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


loadStats : String -> Cmd Msg
loadStats accessToken =
    Http.send StatsLoaded (Api.getAdminStats accessToken)


view : Model -> Document Msg
view m =
    let
        frame { title, content } =
            { title = title
            , body =
                [ content
                ]
            }
    in
    frame <|
        case m.page of
            Stats (Just s) ->
                Dashboard.view SortSchoolsBy s

            Stats Nothing ->
                { title = "Loading Dashboard", content = text "Loading dashboard ..." }

            Error err ->
                { title = "Error loading dashboard", content = text err }

            Login subModel ->
                { title = "Login to 3ml admin", content = Html.map LoginMsg (Login.view subModel Nothing) }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
