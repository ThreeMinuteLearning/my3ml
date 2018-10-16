module Main exposing (main)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Dashboard
import Data.Session exposing (Role(..), User)
import Html exposing (..)
import Http
import Page.Login as Login
import Json.Decode as Decode exposing (Value)


type alias Model =
    { accessToken : Maybe String
    , page : Page
    }

type Page
    = Login Login.Model
    | Stats ( Maybe Dashboard.Stats )



type Msg
    = StatsLoaded (Result Http.Error Value)
    | LoginMsg (Login.Msg Api.Login)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing (Login Login.initialModel ), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( Login subModel, LoginMsg subMsg ) ->
            let
                loginRequest username password otp = Api.postAuthenticate (Api.LoginRequest username password otp)

                ( ( pageModel, loginCmd ), maybeLoggedIn ) =
                    Login.update subMsg subModel loginRequest

                ( newPage, newCmd ) =
                    case maybeLoggedIn of
                        Nothing -> ( Login pageModel, Cmd.map LoginMsg loginCmd )
                        Just u -> ( Stats Nothing, loadStats (.token u) )
            in

            ( { model | accessToken = Maybe.map .token maybeLoggedIn, page = newPage }, newCmd )

        ( Stats _, StatsLoaded (Ok json) ) ->
            let
                stats = Result.toMaybe (Decode.decodeValue Dashboard.decodeStats json)
            in
            ( { model | page = Stats stats }, Cmd.none )

        (_, _ ) ->
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
                Dashboard.view s

            Stats (Nothing) ->
                { title = "Ooops", content =  text "Couldn't load stats" }

            Login subModel ->
                Login.view subModel
                    |> \{ title, content } -> { title = title, content = Html.map LoginMsg content }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
