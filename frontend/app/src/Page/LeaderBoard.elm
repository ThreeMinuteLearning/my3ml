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
        div [ class "max-w-md mx-auto p-4" ]
            [ h1 [ class "text-2xl font-light mb-2" ] [ text "Leaderboard" ]
            , p [ class "mb-6" ]
                [ text "Scores are re-calculated periodically so it may be a while before you see changes."
                ]
            , table [ class "w-full" ]
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
    tr [ classList [ ( "bg-yellow", pos == 0 ), ( "bg-grey-light", pos == 1 ), ( "bg-yellow-darker text-white", pos == 2 ) ] ]
        [ td [ class "text-center p-2" ] [ text (String.fromInt (pos + 1)) ]
        , td [ class "text-center p-2" ] [ text entry.name ]
        , td [ class "text-center p-2" ] [ text (String.fromInt entry.score) ]
        , td [ class "text-center p-2" ] [ text (String.fromInt entry.position) ]
        ]
