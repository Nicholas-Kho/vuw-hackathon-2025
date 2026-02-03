module Navigation exposing
    ( NTNode(..)
    , NavTree
    , addInFlight
    , getTree
    , getTreeWithLoadingNodes
    , insertNeighborsAt
    , singleton
    )

import BackendWrapper exposing (Node, areIdsEqual, unwrapNodeId, wrapNodeId)
import Dict exposing (Dict)
import Generated.BackendApi exposing (NodeId)
import Set exposing (Set)
import Tree exposing (Tree(..))


type NTNode
    = Fetching
    | Loaded Node


type NavTree
    = NavTree
        { tree : Tree ( NodeId, Node )

        -- Careful: These strings are unwrapped NodeIds.
        -- Members are only fetched nodes.
        , members : Set String

        -- Map from node IDs to IDs of in-flight neighbors.
        , inFlight : Dict String (List NodeId)
        , loopsTo : Dict String (Set String)
        }


getTree : NavTree -> Tree ( NodeId, Node )
getTree (NavTree nt) =
    nt.tree


addInFlight : NodeId -> List NodeId -> NavTree -> NavTree
addInFlight parentId childIds (NavTree nt) =
    let
        newCids =
            List.filter
                (\cid ->
                    not <|
                        List.member cid <|
                            Maybe.withDefault [] <|
                                Dict.get (unwrapNodeId parentId) nt.inFlight
                )
                childIds

        alter cids idListM =
            case Maybe.withDefault [] idListM |> List.append cids of
                [] ->
                    Nothing

                x ->
                    Just x

        newInFlight =
            Dict.update (unwrapNodeId parentId) (alter newCids) nt.inFlight
    in
    NavTree { nt | inFlight = newInFlight }


getTreeWithLoadingNodes : NavTree -> Tree ( NodeId, NTNode )
getTreeWithLoadingNodes (NavTree nt) =
    let
        ntMapped =
            Tree.map (Tuple.mapSecond Loaded) nt.tree

        pairFlip a b =
            ( b, a )

        addFetching toIdRaw fetchingIds tree =
            insertHelper tree (wrapNodeId toIdRaw) <| List.map (pairFlip Fetching) fetchingIds
    in
    Dict.foldl addFetching ntMapped nt.inFlight


singleton : ( NodeId, Node ) -> NavTree
singleton n =
    NavTree
        { tree = Node n []
        , members = Set.singleton <| unwrapNodeId <| Tuple.first n
        , loopsTo = Dict.empty
        , inFlight = Dict.empty
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


removeFromInFlight : NodeId -> List NodeId -> NavTree -> NavTree
removeFromInFlight parentId addedIds (NavTree nt) =
    let
        nothingIfEmpty s =
            if List.isEmpty s then
                Nothing

            else
                Just s

        rawIds =
            List.map unwrapNodeId addedIds

        alter idList =
            List.filter (\i -> not <| List.member (unwrapNodeId i) rawIds) idList |> nothingIfEmpty

        newInFlight =
            Dict.update (unwrapNodeId parentId) (Maybe.andThen alter) nt.inFlight
    in
    NavTree { nt | inFlight = newInFlight }


insertNeighborsAt : NavTree -> NodeId -> List ( NodeId, Node ) -> NavTree
insertNeighborsAt nt x ns =
    let
        updateInFlight =
            removeFromInFlight x (List.map Tuple.first ns) nt

        ( alreadyHere, arent ) =
            List.partition (hasNode nt << Tuple.first) ns

        addedMembers =
            List.foldl (addMember << Tuple.first) updateInFlight arent

        (NavTree newNt) =
            List.foldl (addLoop x << Tuple.first) addedMembers alreadyHere

        addedNeighs =
            insertHelper newNt.tree x arent
    in
    NavTree { newNt | tree = addedNeighs }


insertHelper : Tree ( NodeId, a ) -> NodeId -> List ( NodeId, a ) -> Tree ( NodeId, a )
insertHelper (Node c cn) x ns =
    if areIdsEqual (Tuple.first c) x then
        Node c (cn ++ List.map Tree.singleton ns)

    else
        Node c (List.map (\st -> insertHelper st x ns) cn)
