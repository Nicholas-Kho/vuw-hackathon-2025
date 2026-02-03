module BackendWrapper exposing
    ( Node
    , Subgraph
    , addNode
    , areIdsEqual
    , getContent
    , getNode
    , getOutgoing
    , keys
    , makeExpandParams
    , union
    , unwrapNodeId
    , wrapNodeId
    , xformSubgraph
    )

import Dict exposing (Dict)
import Generated.BackendApi exposing (EdgeInfo(..), ExpandParams, NodeContent, NodeElm, NodeId(..), Subgraph, UnverifiedNodeId(..))
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


getOutgoing : Node -> List NodeId
getOutgoing (Node n) =
    n.outgoingEdges |> Dict.keys |> List.map wrapNodeId


makeExpandParams : NodeId -> ExpandParams
makeExpandParams nid =
    { expandAboutId = UnverifiedNodeId { toString = unwrapNodeId nid } }


type Subgraph
    = Subgraph (Dict String Node)


keys : Subgraph -> List NodeId
keys (Subgraph d) =
    Dict.keys d |> List.map wrapNodeId


union : Subgraph -> Subgraph -> Subgraph
union (Subgraph a) (Subgraph b) =
    Subgraph <| Dict.union a b


getNode : Subgraph -> NodeId -> Maybe Node
getNode (Subgraph d) nid =
    Dict.get (unwrapNodeId nid) d


addNode : Subgraph -> NodeId -> Node -> Subgraph
addNode (Subgraph d) nid node =
    Subgraph (Dict.insert (unwrapNodeId nid) node d)


unwrapNodeId : NodeId -> String
unwrapNodeId (NodeId nid) =
    nid.content


wrapNodeId : String -> NodeId
wrapNodeId s =
    NodeId { content = s }


areIdsEqual : NodeId -> NodeId -> Bool
areIdsEqual (NodeId a) (NodeId b) =
    a.content == b.content


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
