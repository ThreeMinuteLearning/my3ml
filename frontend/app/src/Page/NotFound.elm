module Page.NotFound exposing (view)

import Data.Session exposing (Session)
import Html exposing (Html, main_, h1, div, img, text)
import Html.Attributes exposing (class, tabindex, id, src, alt)


view : Session -> { title: String, content: Html msg }
view session =
    { title = "Page Not Found"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Not Found" ]
            , div [ class "row" ]
                [ text "not found" ]
            ]
    }
