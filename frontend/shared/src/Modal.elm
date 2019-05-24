module Modal exposing (view)

import Bootstrap
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


view : String -> msg -> Html msg -> Html msg
view title onClose content =
    div [ class "fixed inset-0 bg-gray-100" ]
        [ Bootstrap.closeBtn2 onClose
        , h1 [ class "text-xl text-gray-700 text-center border-b shadow py-4 mb-4" ] [ text title ]
        , div [ class "h-full flex justify-center mt-8" ]
            [ content
            ]
        ]
