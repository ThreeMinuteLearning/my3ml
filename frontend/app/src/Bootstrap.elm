module Bootstrap exposing (Alert(..), alert, btn, btnGroup, closeBtn, closeBtn2, errorClass, formGroup, link, row, tableCustomizations, toolbar)

import Components
import Html exposing (Attribute, Html, button, div, span, text, tr)
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


closeBtn msg =
    span [ class "absolute pin-t pin-r px-4 py-3", ariaLabel "Close", onClick msg ]
        [ closeIcon ]


closeBtn2 msg =
    span [ class "absolute pin-t pin-r p-4", onClick msg ]
        [ closeIcon2 ]


closeIcon =
    Svg.svg [ Svga.class "fill-current h-6 w-6", role "button", Svga.viewBox "0 0 20 20" ]
        [ Svg.title [] [ Svg.text "Close" ]
        , Svg.path [ Svga.d "M14.348 14.849a1.2 1.2 0 0 1-1.697 0L10 11.819l-2.651 3.029a1.2 1.2 0 1 1-1.697-1.697l2.758-3.15-2.759-3.152a1.2 1.2 0 1 1 1.697-1.697L10 8.183l2.651-3.031a1.2 1.2 0 1 1 1.697 1.697l-2.758 3.152 2.758 3.15a1.2 1.2 0 0 1 0 1.698z" ] []
        ]


closeIcon2 =
    Svg.svg [ Svga.class "h-12 w-12 text-grey-dark hover:text-grey-darker fill-current", role "button", Svga.viewBox "0 0 20 20" ]
        [ Svg.title [] [ Svg.text "Close" ]
        , Svg.path [ Svga.d "M14.348 14.849a1.2 1.2 0 0 1-1.697 0L10 11.819l-2.651 3.029a1.2 1.2 0 1 1-1.697-1.697l2.758-3.15-2.759-3.152a1.2 1.2 0 1 1 1.697-1.697L10 8.183l2.651-3.031a1.2 1.2 0 1 1 1.697 1.697l-2.758 3.152 2.758 3.15a1.2 1.2 0 0 1 0 1.698z" ] []
        ]


toolbar id_ =
    div [ id id_, class "btn-toolbar hidden-print", role "toolbar" ]


btnGroup =
    div [ class "btn-group", role "group" ]


link : Attribute msg -> String -> Html msg
link href_ txt =
    Html.a [ class "text-blue hover:text-blue-dark no-underline", href_ ] [ text txt ]


btn : String -> msg -> String -> Html msg
btn id_ action txt =
    Components.btn [ id id_, onClick action, type_ "button" ] [ text txt ]


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
