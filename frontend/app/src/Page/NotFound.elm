module Page.NotFound exposing (view)

import Data.Session exposing (Session)
import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)


view : Session -> { title : String, content : Html msg }
view session =
    { title = "Page Not Found"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [ class "text-xl font-light" ] [ text "Not Found" ]
            , div [ class "row" ]
                [ text "not found" ]
            ]
    }
