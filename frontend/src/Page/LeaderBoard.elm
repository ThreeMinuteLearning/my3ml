module Page.LeaderBoard exposing (Model, init, view)

import Api
import Data.Session as Session exposing (Session, authorization)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Errored exposing (PageLoadError(..), pageLoadError)
import Task exposing (Task)
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


view : Model -> Html msg
view model =
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
                (List.map tableRow (List.indexedMap (,) model))
            ]
        ]


tableRow : ( Int, Api.LeaderBoardEntry ) -> Html msg
tableRow ( pos, entry ) =
    tr []
        [ td [] [ text (toString (pos + 1)) ]
        , td [] [ text entry.name ]
        , td [] [ text (toString entry.score) ]
        , td [] [ text (toString entry.position) ]
        ]
