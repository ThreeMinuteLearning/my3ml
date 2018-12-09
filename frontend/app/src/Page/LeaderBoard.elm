module Page.LeaderBoard exposing (Model, init, view)

import Api
import Data.Session as Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Task exposing (Task)
import Tuple exposing (pair)
import Util exposing (defaultHttpErrorMsg)


type alias Model =
    List Api.LeaderBoardEntry


init : Session -> Task PageLoadError Model
init session =
    let
        handleLoadError e =
            pageLoadError e (defaultHttpErrorMsg e)
    in
    Api.getSchoolLeaderboard (authorization session)
        |> Http.toTask
        |> Task.mapError handleLoadError


view : Model -> { title : String, content : Html msg }
view model =
    { title = "Leaderboard"
    , content =
        div [ class "container page" ]
            [ table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Position" ]
                        , th [] [ text "Name" ]
                        , th [] [ text "Score" ]
                        , th [] [ text "Overall 3ml Position" ]
                        ]
                    ]
                , tbody []
                    (List.map tableRow (List.indexedMap pair model))
                ]
            ]
    }


tableRow : ( Int, Api.LeaderBoardEntry ) -> Html msg
tableRow ( pos, entry ) =
    tr []
        [ td [] [ text (String.fromInt (pos + 1)) ]
        , td [] [ text entry.name ]
        , td [] [ text (String.fromInt entry.score) ]
        , td [] [ text (String.fromInt entry.position) ]
        ]
