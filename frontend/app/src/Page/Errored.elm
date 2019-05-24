module Page.Errored exposing (PageLoadError(..), pageLoadError, view)

{-| The page that renders when there was an error trying to load another page.
-}

import Html exposing (..)
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
            [ div [ class "row" ]
                [ h1 [ class "text-lg font-light" ] [ text "Error Loading Page" ]
                , case error of
                    PageLoadError msg ->
                        p [] [ text msg ]

                    AuthenticationRequired ->
                        p [] [ text "It looks like your session may have expired. Please ", a [ Route.href Route.Login ] [ text "log back in." ] ]
                ]
            ]
    }
