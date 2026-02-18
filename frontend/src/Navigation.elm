module Navigation exposing
    ( NTNode(..)
    , NavTree
    , addInFlight
    , getLayout
    , getLoopsFrom
    , getTree
    , getTreeWithLoadingNodes
    , hasId
    , insertFetchResults
    , insertNeighborsAt
    , isFrontier
    , recomputeMemo
    , singleton
    )

import BackendWrapper exposing (Node, Subgraph, areIdsEqual, getId, getNode, getOutgoing, unwrapNodeId, wrapNodeId)
import Dict exposing (Dict)
import Generated.BackendApi exposing (NodeId)
import Set exposing (Set)
import Tree exposing (Tree(..), WithPos)


type NTNode
    = Fetching
    | Loaded Node


hasId : NodeId -> NTNode -> Bool
hasId nid ntn =
    case ntn of
        Fetching ->
            False

        Loaded n ->
            nid == getId n


type NavTree
    = NavTree
        { tree : Tree Node

        -- Careful: These strings are unwrapped NodeIds.
        -- Members are only fetched nodes.
        , members : Set String

        -- Map from node IDs to IDs of in-flight neighbors.
        , inFlight : Dict String (List NodeId)
        , loopsFrom : Dict String (Set String)

        -- These are used for rendering and click detection. It can be computed on the fly,
        -- but these function calls are not cheap so we memoize them. Otherwise, they
        -- get recomputed each frame, which can cause slow-down and jittering with large trees.
        , memoFullTree : Tree NTNode
        , memoFullTreeLayout : Tree (WithPos NTNode)
        }


getTree : NavTree -> Tree Node
getTree (NavTree nt) =
    nt.tree


getInFlight : NavTree -> Dict String (List NodeId)
getInFlight (NavTree nt) =
    nt.inFlight


getLayout : NavTree -> Tree (WithPos NTNode)
getLayout (NavTree nt) =
    nt.memoFullTreeLayout


getLoopsFrom : NavTree -> NodeId -> List NodeId
getLoopsFrom (NavTree nt) nid =
    Dict.get (unwrapNodeId nid) nt.loopsFrom
        |> Maybe.withDefault Set.empty
        |> Set.toList
        |> List.map wrapNodeId


isFrontier : NavTree -> Node -> Bool
isFrontier (NavTree nt) node =
    let
        nidRaw =
            unwrapNodeId <| getId node

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
            List.filterMap (getNode sg) cids
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


getTreeWithLoadingNodes : NavTree -> Tree NTNode
getTreeWithLoadingNodes (NavTree nt) =
    nt.memoFullTree


makeFullTreeHelper : Dict String (List NodeId) -> Tree Node -> Tree NTNode
makeFullTreeHelper outgoingNodes (Node currentNode children) =
    let
        convertNode =
            Loaded currentNode

        loadingChildren =
            Dict.get (unwrapNodeId <| getId currentNode) outgoingNodes
                |> Maybe.withDefault []
                |> List.map (\_ -> Tree.singleton Fetching)

        recurseChildren =
            List.map (makeFullTreeHelper outgoingNodes) children
    in
    Node convertNode (recurseChildren ++ loadingChildren)


recomputeMemo : NavTree -> NavTree
recomputeMemo (NavTree nt) =
    let
        newFullTree =
            makeFullTreeHelper nt.inFlight nt.tree

        newLayout =
            Tree.layoutTree newFullTree
    in
    NavTree
        { nt
            | memoFullTree = newFullTree
            , memoFullTreeLayout = newLayout
        }


singleton : Node -> NavTree
singleton n =
    let
        memoTree =
            Node (Loaded n) []
    in
    NavTree
        { tree = Node n []
        , members = Set.singleton <| unwrapNodeId <| getId n
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


insertNeighborsAt : NavTree -> NodeId -> List Node -> NavTree
insertNeighborsAt nt x ns =
    let
        updateInFlight =
            removeFromInFlight x (List.map getId ns) nt

        ( alreadyHere, arent ) =
            List.partition (hasNode nt << getId) ns

        addedMembers =
            List.foldl (addMember << getId) updateInFlight arent

        (NavTree newNt) =
            List.foldl (addLoop x << getId) addedMembers alreadyHere

        addedNeighs =
            insertHelper newNt.tree x arent
    in
    NavTree { newNt | tree = addedNeighs }


insertHelper : Tree Node -> NodeId -> List Node -> Tree Node
insertHelper (Node c cn) x ns =
    if areIdsEqual (getId c) x then
        Node c (cn ++ List.map Tree.singleton ns)

    else
        Node c (List.map (\st -> insertHelper st x ns) cn)
