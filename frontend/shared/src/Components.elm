module Components exposing (btn)

import Html exposing (Attribute, Html, button, div, span, text, tr)
import Html.Attributes exposing (attribute, class, id, type_)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as Svga


btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn attrs =
    button (class "bg-blue hover:bg-blue-dark text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline" :: attrs)
