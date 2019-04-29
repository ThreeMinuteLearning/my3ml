module Page.Errored exposing (PageLoadError(..), pageLoadError, view)

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.
-}

import Html exposing (Html, a, div, h3, img, main_, p, text)
import Html.Attributes exposing (class, id, tabindex)
import Http
import Route


type PageLoadError
    = PageLoadError String
    | AuthenticationRequired


pageLoadError : Http.Error -> String -> PageLoadError
pageLoadError err msg =
    case err of
        Http.BadStatus r ->
            case .code r.status of
                401 ->
                    AuthenticationRequired

                _ ->
                    PageLoadError msg

        _ ->
            PageLoadError msg


view : PageLoadError -> { title : String, content : Html msg }
view error =
    { title = "Error loading page"
    , content =
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
    }
