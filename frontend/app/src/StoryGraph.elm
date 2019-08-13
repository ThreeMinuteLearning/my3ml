module StoryGraph exposing (StoryGraph, empty, fromStoriesAndConnections, storyContext)

import Api
import Graph exposing (Edge, Graph, Node)
import IntDict


type StoryGraph
    = StoryGraph (Graph Api.Story String)


empty : StoryGraph
empty =
    StoryGraph Graph.empty


fromStoriesAndConnections : List Api.Story -> List Api.GraphEdge -> StoryGraph
fromStoriesAndConnections stories edges =
    Graph.fromNodesAndEdges (List.map (\s -> Node s.id s) stories) (List.map (\e -> Edge e.from e.to e.description) edges)
        |> StoryGraph


storyContext : Api.Story -> StoryGraph -> ( List ( Api.Story, String ), List ( Api.Story, String ) )
storyContext story (StoryGraph graph) =
    let
        context =
            Graph.get story.id graph

        idToStory ( id, label ) =
            Graph.get id graph
                |> Maybe.map (\ctx -> ( ctx.node.label, label ))

        incoming =
            context
                |> Maybe.map .incoming
                |> Maybe.map IntDict.toList
                |> Maybe.withDefault []
                |> List.filterMap idToStory

        outgoing =
            context
                |> Maybe.map .outgoing
                |> Maybe.map IntDict.toList
                |> Maybe.withDefault []
                |> List.filterMap idToStory
    in
    ( incoming, outgoing )
