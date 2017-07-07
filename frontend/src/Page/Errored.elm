module Page.Errored exposing (view, pageLoadError, PageLoadError(..))

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.
-}

import Data.Session as Session exposing (Session)
import Html exposing (Html, main_, h3, div, img, text, p, a)
import Html.Attributes exposing (class, tabindex, id)
import Http
import Route


type PageLoadError
    = PageLoadError String
    | AuthenticationRequired


pageLoadError : Http.Error -> String -> PageLoadError
pageLoadError err msg =
    case err of
        Http.BadStatus r ->
            case (.code r.status) of
                401 ->
                    AuthenticationRequired

                _ ->
                    PageLoadError msg

        _ ->
            PageLoadError msg


view : Session -> PageLoadError -> Html msg
view session error =
    main_ [ id "content", class "container", tabindex -1 ]
        (case error of
            PageLoadError msg ->
                [ div [ class "row" ]
                    [ h3 [] [ text "Error Loading Page" ]
                    , p [] [ text msg ]
                    ]
                ]

            AuthenticationRequired ->
                [ div [ class "row" ]
                    [ h3 [] [ text "Login Required" ]
                    , p [] [ text "It looks like your session may have expired. Please ", a [ Route.href Route.Login ] [ text "log back in." ] ]
                    ]
                ]
        )
