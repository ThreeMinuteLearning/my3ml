module Bootstrap exposing (errorClass, tableCustomizations, toolbar, btnGroup, btn, closeBtn, alert, Alert(..))

import Html exposing (Html, button, div, span, text, tr)
import Html.Attributes exposing (attribute, id, class, type_)
import Html.Events exposing (onClick)
import Table


type Alert
    = Success
    | Danger


alert : Alert -> String -> msg -> Html msg
alert a txt dismiss =
    let
        alertClass =
            case a of
                Success ->
                    "alert-success"

                Danger ->
                    "alert-danger"
    in
        div [ class ("alert alert-dismissable " ++ alertClass), role "alert" ]
            [ closeBtn dismiss
            , text txt
            ]


toolbar id_ =
    div [ id id_, class "btn-toolbar hidden-print", role "toolbar" ]


btnGroup =
    div [ class "btn-group", role "group" ]


btn action =
    button [ class "btn btn-default", onClick action ]


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


headWithTr =
    c.thead
        >> .children
        >> tr []
        >> List.singleton
        >> Table.HtmlDetails []


tableCustomizations =
    { c | thead = headWithTr, tableAttrs = [ class "table table-striped" ] }
