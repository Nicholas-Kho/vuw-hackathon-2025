module Navigation exposing
    ( NavTree
    , getTree
    , insertNeighborsAt
    , singleton
    )

import BackendWrapper exposing (Node, unwrapNodeId)
import Dict exposing (Dict)
import Generated.BackendApi exposing (NodeId)
import Set exposing (Set)
import Tree exposing (Tree(..))


type NavTree
    = NavTree
        { tree : Tree ( NodeId, Node )

        -- Careful: These strings are unwrapped NodeIds.
        , members : Set String
        , loopsTo : Dict String (Set String)
        }


getTree : NavTree -> Tree ( NodeId, Node )
getTree (NavTree nt) =
    nt.tree


singleton : ( NodeId, Node ) -> NavTree
singleton n =
    NavTree
        { tree = Node n []
        , members = Set.singleton <| unwrapNodeId <| Tuple.first n
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


insertNeighborsAt : NavTree -> NodeId -> List ( NodeId, Node ) -> NavTree
insertNeighborsAt nt x ns =
    let
        ( alreadyHere, arent ) =
            List.partition (hasNode nt << Tuple.first) ns

        addedMembers =
            List.foldl (addMember << Tuple.first) nt arent

        (NavTree newNt) =
            List.foldl (addLoop x << Tuple.first) addedMembers alreadyHere

        addedNeighs =
            insertHelper newNt.tree x arent
    in
    NavTree { newNt | tree = addedNeighs }


insertHelper : Tree ( NodeId, Node ) -> NodeId -> List ( NodeId, Node ) -> Tree ( NodeId, Node )
insertHelper (Node c cn) x ns =
    if unwrapNodeId (Tuple.first c) == unwrapNodeId x then
        Node c (cn ++ List.map Tree.singleton ns)

    else
        Node c (List.map (\st -> insertHelper st x ns) cn)
