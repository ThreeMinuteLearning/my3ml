module Bootstrap exposing (tableCustomizations, toolbar, btnGroup, btn)

import Html exposing (button, div, tr)
import Html.Attributes exposing (attribute, id, class)
import Html.Events exposing (onClick)
import Table


toolbar id_ =
    div [ id id_, class "btn-toolbar", role "toolbar" ]


btnGroup =
    div [ class "btn-group", role "group" ]


btn action =
    button [ class "btn btn-default", onClick action ]


role =
    attribute "role"


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
