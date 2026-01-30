module Navigation exposing
    ( NavTree
    , getTree
    , insertNeighborsAt
    , singleton
    , toNodeTree
    )

import BackendWrapper exposing (Node, Subgraph, getNode, unwrapNodeId)
import Dict exposing (Dict)
import Generated.BackendApi exposing (NodeId)
import Set exposing (Set)
import Tree exposing (Tree(..))


type NavTree
    = NavTree
        { tree : Tree NodeId

        -- Careful: These strings are unwrapped NodeIds.
        , members : Set String
        , loopsTo : Dict String (Set String)
        }


getTree : NavTree -> Tree NodeId
getTree (NavTree nt) =
    nt.tree


singleton : NodeId -> NavTree
singleton nid =
    NavTree
        { tree = Node nid []
        , members = Set.singleton <| unwrapNodeId nid
        , loopsTo = Dict.empty
        }


hasNode : NavTree -> NodeId -> Bool
hasNode (NavTree nt) nid =
    Set.member (unwrapNodeId nid) nt.members


addLoop : NodeId -> NodeId -> NavTree -> NavTree
addLoop from to (NavTree nt) =
    let
        oldSet =
            Dict.get (unwrapNodeId to) nt.loopsTo |> Maybe.withDefault Set.empty

        newSet =
            Set.insert (unwrapNodeId from) oldSet

        newLoops =
            Dict.insert (unwrapNodeId to) newSet nt.loopsTo
    in
    NavTree { nt | loopsTo = newLoops }


addMember : NodeId -> NavTree -> NavTree
addMember x (NavTree nt) =
    NavTree { nt | members = Set.insert (unwrapNodeId x) nt.members }


toNodeTree : Subgraph -> NavTree -> Tree (Maybe Node)
toNodeTree sg (NavTree nt) =
    Tree.map (getNode sg) nt.tree


insertNeighborsAt : NavTree -> NodeId -> List NodeId -> NavTree
insertNeighborsAt nt x ns =
    let
        ( alreadyHere, arent ) =
            List.partition (hasNode nt) ns

        addedMembers =
            List.foldl addMember nt arent

        (NavTree newNt) =
            List.foldl (addLoop x) addedMembers alreadyHere

        addedNeighs =
            insertHelper newNt.tree x arent
    in
    NavTree { newNt | tree = addedNeighs }


insertHelper : Tree NodeId -> NodeId -> List NodeId -> Tree NodeId
insertHelper (Node c cn) x ns =
    if unwrapNodeId c == unwrapNodeId x then
        Node c (cn ++ List.map Tree.singleton ns)

    else
        Node c (List.map (\st -> insertHelper st x ns) cn)
