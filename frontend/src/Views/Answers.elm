module Views.Answers exposing (view, viewWithStories)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)


view : List Api.Answer -> Html msg
view answers =
    div [ class "hidden-print" ]
        ([ h3 [] [ text "Story Answers" ] ]
            ++ (List.map viewAnswer answers)
        )


viewAnswer : Api.Answer -> Html msg
viewAnswer answer =
    div [ class "row" ]
        [ div [ class "col-md-9" ]
            [ p [] [ text answer.connect ]
            , p [] [ text answer.question ]
            , p [] [ text answer.summarise ]
            , p [] [ text answer.clarify ]
            ]
        ]


viewWithStories : List ( Api.Answer, Api.Story ) -> Html msg
viewWithStories answers =
    div []
        ([ h3 [] [ text "Story Answers" ] ]
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
            , p [] [ text answer.clarify ]
            ]
        ]
