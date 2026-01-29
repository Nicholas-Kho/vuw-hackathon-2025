module BackendWrapper exposing
    ( Node
    , Subgraph
    , xformSubgraph
    )

import Dict exposing (Dict)
import Generated.BackendApi exposing (EdgeInfo(..), NodeContent, NodeElm, NodeId(..), Subgraph)
import Set exposing (Set)


type Node
    = Node
        { content : NodeContent
        , outgoingEdges : Dict String (Set String)
        , incomingEdges : Dict String (Set String)
        }


getContent : Node -> NodeContent
getContent (Node n) =
    n.content


type Subgraph
    = Subgraph (Dict String Node)


unwrapNodeId : NodeId -> String
unwrapNodeId (NodeId nid) =
    nid.content


unwrapEdgeInfo : EdgeInfo -> String
unwrapEdgeInfo (EdgeInfo inf) =
    inf.text


xformNode : NodeElm -> Node
xformNode ne =
    let
        edgeInfoToSet =
            Set.fromList << List.map unwrapEdgeInfo

        unwrapAndDict =
            Dict.fromList << List.map (\( nid, x ) -> ( unwrapNodeId nid, edgeInfoToSet x ))
    in
    Node
        { content = ne.content
        , outgoingEdges = unwrapAndDict ne.outgoingEdges
        , incomingEdges = unwrapAndDict ne.incomingEdges
        }


xformSubgraph : Generated.BackendApi.Subgraph -> Subgraph
xformSubgraph sg =
    let
        xform =
            List.map (\( nid, n ) -> ( unwrapNodeId nid, xformNode n )) sg.contents
    in
    Subgraph <| Dict.fromList <| xform
