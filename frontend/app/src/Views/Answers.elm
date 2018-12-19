module Views.Answers exposing (view, viewWithStories)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)


view : Api.Story -> List Api.Answer -> List (Html msg)
view story answers =
    List.map (viewAnswer story) answers


viewAnswer : Api.Story -> Api.Answer -> Html msg
viewAnswer story answer =
    div [ class "rounded shadow-md mt-2 py-2" ] (viewDetails story answer)


viewWithStories : List ( Api.Answer, Api.Story ) -> Html msg
viewWithStories answers =
    div []
        (List.map viewStoryAnswer answers)


viewStoryAnswer : ( Api.Answer, Api.Story ) -> Html msg
viewStoryAnswer ( answer, story ) =
    div [ class "rounded shadow-md mt-2 py-2" ]
        (h2 [ class "text-xl text-center font-light" ] [ text story.title ] :: viewDetails story answer)


viewDetails : Api.Story -> Api.Answer -> List (Html msg)
viewDetails story answer =
    [ cqsc "Connect" [ text answer.connect ]
    , cqsc "Question" [ text answer.question ]
    , cqsc "Summarise" [ text answer.summarise ]
    , cqsc "Clarify" [ em [] [ text story.clarifyWord ], text ": ", text answer.clarify ]
    ]


cqsc : String -> List (Html msg) -> Html msg
cqsc answerType content =
    div [ class "px-6 py-2" ]
        (h3 [ class "text-base font-bold text-grey-dark" ] [ text answerType ] :: content)
