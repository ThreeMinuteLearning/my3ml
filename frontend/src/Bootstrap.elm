module Bootstrap exposing (errorClass, tableCustomizations, toolbar, btnGroup, btn, submitButton)

import Form
import Html exposing (Html, button, div, text, tr)
import Html.Attributes exposing (attribute, id, class, type_)
import Html.Events exposing (onClick)
import Table


submitButton : String -> Html Form.Msg
submitButton txt =
    button [ class "btn btn-primary", type_ "submit", onClick Form.Submit ] [ text txt ]


toolbar id_ =
    div [ id id_, class "btn-toolbar", role "toolbar" ]


btnGroup =
    div [ class "btn-group", role "group" ]


btn action =
    button [ class "btn btn-default", onClick action ]


role =
    attribute "role"


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
