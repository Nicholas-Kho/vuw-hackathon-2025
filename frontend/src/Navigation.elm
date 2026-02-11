module Navigation exposing
    ( NTNode(..)
    , NavTree
    , addInFlight
    , getLayout
    , getLoopsFrom
    , getTree
    , getTreeWithLoadingNodes
    , insertFetchResults
    , insertNeighborsAt
    , isFrontier
    , recomputeMemo
    , singleton
    )

import BackendWrapper exposing (Node, Subgraph, areIdsEqual, getNode, getOutgoing, unwrapNodeId, wrapNodeId)
import Dict exposing (Dict)
import Generated.BackendApi exposing (NodeId)
import Set exposing (Set)
import Tree exposing (Tree(..), WithPos)


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
        , loopsFrom : Dict String (Set String)

        -- These are used for rendering and click detection. It can be computed on the fly,
        -- but these function calls are not cheap so we memoize them. Otherwise, they
        -- get recomputed each frame, which can cause slow-down and jittering with large trees.
        , memoFullTree : Tree ( NodeId, NTNode )
        , memoFullTreeLayout : Tree (WithPos ( NodeId, NTNode ))
        }


getTree : NavTree -> Tree ( NodeId, Node )
getTree (NavTree nt) =
    nt.tree


getInFlight : NavTree -> Dict String (List NodeId)
getInFlight (NavTree nt) =
    nt.inFlight


getLayout : NavTree -> Tree (WithPos ( NodeId, NTNode ))
getLayout (NavTree nt) =
    nt.memoFullTreeLayout


getLoopsFrom : NavTree -> NodeId -> List NodeId
getLoopsFrom (NavTree nt) nid =
    Dict.get (unwrapNodeId nid) nt.loopsFrom
        |> Maybe.withDefault Set.empty
        |> Set.toList
        |> List.map wrapNodeId


isFrontier : NavTree -> ( NodeId, Node ) -> Bool
isFrontier (NavTree nt) ( nid, node ) =
    let
        nidRaw =
            unwrapNodeId nid

        noInFlightNeighs nr =
            Dict.get nr nt.inFlight
                |> Maybe.map List.isEmpty
                |> Maybe.withDefault True

        hasOutOfTreeNeigh n =
            n
                |> getOutgoing
                |> List.any (\nidprime -> not <| Set.member (unwrapNodeId nidprime) nt.members)
    in
    Set.member nidRaw nt.members && noInFlightNeighs nidRaw && hasOutOfTreeNeigh node


insertFetchResults : Subgraph -> NavTree -> NavTree
insertFetchResults sg nt =
    let
        lookupAndAdd pid cids navTree =
            List.filterMap (\cid -> getNode sg cid |> Maybe.map (Tuple.pair cid)) cids
                |> insertNeighborsAt navTree (wrapNodeId pid)
    in
    Dict.foldl lookupAndAdd nt (getInFlight nt)


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
    nt.memoFullTree


recomputeMemo : NavTree -> NavTree
recomputeMemo (NavTree nt) =
    let
        ntMapped =
            Tree.map (Tuple.mapSecond Loaded) nt.tree

        pairFlip a b =
            ( b, a )

        addFetching toIdRaw fetchingIds tree =
            insertHelper tree (wrapNodeId toIdRaw) <| List.map (pairFlip Fetching) fetchingIds

        newFullTree =
            Dict.foldl addFetching ntMapped nt.inFlight

        newLayout =
            Tree.layoutTree newFullTree
    in
    NavTree
        { nt
            | memoFullTree = newFullTree
            , memoFullTreeLayout = newLayout
        }


singleton : ( NodeId, Node ) -> NavTree
singleton n =
    let
        memoTree =
            Node (Tuple.mapSecond Loaded n) []
    in
    NavTree
        { tree = Node n []
        , members = Set.singleton <| unwrapNodeId <| Tuple.first n
        , loopsFrom = Dict.empty
        , inFlight = Dict.empty
        , memoFullTree = memoTree
        , memoFullTreeLayout = Tree.layoutTree memoTree
        }


hasNode : NavTree -> NodeId -> Bool
hasNode (NavTree nt) nid =
    Set.member (unwrapNodeId nid) nt.members


addLoop : NodeId -> NodeId -> NavTree -> NavTree
addLoop from to (NavTree nt) =
    let
        oldSet =
            Dict.get (unwrapNodeId from) nt.loopsFrom |> Maybe.withDefault Set.empty

        newSet =
            Set.insert (unwrapNodeId to) oldSet

        newLoops =
            Dict.insert (unwrapNodeId from) newSet nt.loopsFrom
    in
    NavTree { nt | loopsFrom = newLoops }


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
