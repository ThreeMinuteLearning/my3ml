module Bootstrap exposing (Alert(..), alert, btn, btnGroup, closeBtn, errorClass, formGroup, row, tableCustomizations, toolbar)

import Html exposing (Html, button, div, span, text, tr)
import Html.Attributes exposing (attribute, class, id, type_)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as Svga
import Table


type Alert
    = Success
    | Danger


row : List (Html msg) -> Html msg
row =
    div [ class "" ]


formGroup : List (Html msg) -> Html msg
formGroup =
    div [ class "form-group" ]


alert : Alert -> String -> msg -> Html msg
alert a txt dismiss =
    let
        alertClass =
            case a of
                Success ->
                    "bg-green-lightest border border-green-light text-green-dark px-4 py-3 rounded relative"

                Danger ->
                    "bg-red-lightest border border-red-light text-red-dark px-4 py-3 rounded relative"
    in
    div [ class alertClass, role "alert" ]
        [ span [ class "block" ] [ text txt ]
        , closeBtn2 dismiss
        ]


closeBtn2 msg =
    span [ class "absolute pin-t pin-b pin-r px-4 py-3", onClick msg ]
        [ Svg.svg [ Svga.class "fill-current h-6 w-6", role "button", Svga.viewBox "0 0 20 20" ]
            [ Svg.title [] [ Svg.text "Close" ]
            , Svg.path [ Svga.d "M14.348 14.849a1.2 1.2 0 0 1-1.697 0L10 11.819l-2.651 3.029a1.2 1.2 0 1 1-1.697-1.697l2.758-3.15-2.759-3.152a1.2 1.2 0 1 1 1.697-1.697L10 8.183l2.651-3.031a1.2 1.2 0 1 1 1.697 1.697l-2.758 3.152 2.758 3.15a1.2 1.2 0 0 1 0 1.698z" ] []
            ]
        ]


toolbar id_ =
    div [ id id_, class "btn-toolbar hidden-print", role "toolbar" ]


btnGroup =
    div [ class "btn-group", role "group" ]


btn : String -> msg -> String -> Html msg
btn id_ action txt =
    button [ id id_, class "h-8 bg-blue hover:bg-blue-dark text-white text-base font-bold py-2 px-4 rounded", type_ "button", onClick action ] [ text txt ]


closeBtn action =
    button [ class "close", ariaLabel "Close", onClick action ] [ span [ ariaHidden ] [ text "×" ] ]


role =
    attribute "role"


ariaLabel =
    attribute "aria-label"


ariaHidden =
    attribute "aria-hidden" "true"


errorClass : Maybe e -> String
errorClass maybeError =
    Maybe.map (\_ -> "has-error") maybeError |> Maybe.withDefault ""


c =
    Table.defaultCustomizations


tableCustomizations =
    { c | tableAttrs = [ class "sortable-table" ] }
