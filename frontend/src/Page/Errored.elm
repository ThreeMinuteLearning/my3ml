module Page.Errored exposing (view, pageLoadError, PageLoadError)

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.
-}

import Data.Session as Session exposing (Session)
import Html exposing (Html, main_, h3, div, img, text, p)
import Html.Attributes exposing (class, tabindex, id, alt)


type PageLoadError
    = PageLoadError Model


type alias Model =
    { errorMessage : String
    }


pageLoadError : String -> PageLoadError
pageLoadError errorMessage =
    PageLoadError { errorMessage = errorMessage }


view : Session -> PageLoadError -> Html msg
view session (PageLoadError model) =
    main_ [ id "content", class "container", tabindex -1 ]
        [ h3 [] [ text "Error Loading Page" ]
        , div [ class "row" ]
            [ p [] [ text model.errorMessage ] ]
        ]
