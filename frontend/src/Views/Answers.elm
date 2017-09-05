module Views.Answers exposing (view, viewWithStories)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)


view : Api.Story -> List Api.Answer -> Html msg
view story answers =
    div [ class "hidden-print" ]
        ((h3 [] [ text "Story answers" ])
            :: (List.map (viewAnswer story) answers)
        )


viewAnswer : Api.Story -> Api.Answer -> Html msg
viewAnswer story answer =
    div [ class "row" ]
        [ div [ class "col-md-9" ]
            [ p [] [ text answer.connect ]
            , p [] [ text answer.question ]
            , p [] [ text answer.summarise ]
            , p [] (clarifyText story answer)
            ]
        ]


viewWithStories : List ( Api.Answer, Api.Story ) -> Html msg
viewWithStories answers =
    div []
        ([ h3 [] [ text "Story answers" ] ]
            ++ (List.map viewStoryAnswer answers)
        )


viewStoryAnswer : ( Api.Answer, Api.Story ) -> Html msg
viewStoryAnswer ( answer, story ) =
    div [ class "row" ]
        [ div [ class "col-md-9" ]
            [ h4 [] [ text story.title ]
            , p [] [ text answer.connect ]
            , p [] [ text answer.question ]
            , p [] [ text answer.summarise ]
            , p [] (clarifyText story answer)
            ]
        ]


clarifyText : Api.Story -> Api.Answer -> List (Html msg)
clarifyText story answer =
    [ em [] [ text story.clarifyWord ], text ": ", text answer.clarify ]
