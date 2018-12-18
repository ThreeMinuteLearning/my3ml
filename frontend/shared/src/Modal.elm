module Modal exposing (view)

import Bootstrap
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


view : String -> msg -> Html msg -> Html msg
view title onClose content =
    div [ class "fixed pin flex items-center justify-center bg-grey-lightest" ]
        [ Bootstrap.closeBtn2 onClose
        , content
        ]
