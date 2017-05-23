module Views.Spinner exposing (spinner)

import Html exposing (Html, div, li)
import Html.Attributes exposing (class)


spinner : Html msg
spinner =
    li [ class "spinner" ] []
