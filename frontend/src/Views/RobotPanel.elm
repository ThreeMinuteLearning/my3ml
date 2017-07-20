module Views.RobotPanel exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    div [ class "hidden-print panel panel-default" ]
        [ img [ src "img/robot.png" ] []
        ]
