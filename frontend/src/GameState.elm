module GameState exposing (..)

import BackendWrapper exposing (Node, Subgraph, getContent, getNode, getOutgoing, xformSubgraph)
import Camera exposing (Camera, Vec2, focusOn, moveCam, stopAnimation, tickCam, vDistSqare, zoomAbout)
import Generated.BackendApi exposing (InitialGameState, NodeContent, NodeId)
import List exposing (foldl)
import Navigation exposing (NTNode(..), NavTree, addInFlight, getTreeWithLoadingNodes, insertFetchResults, insertNeighborsAt)
import PlayerInput exposing (UserInput(..))
import Tree exposing (WithPos, layoutTree)


type alias GameState =
    { startAt : NodeId
    , endAt : NodeId
    , nodeCache : Subgraph
    , nav : NavTree
    , focus : NodeContent
    , cam : Camera
    }


fromInitial : Node -> Camera -> InitialGameState -> GameState
fromInitial initialFocus cam igs =
    { startAt = igs.startAt
    , endAt = igs.endAt
    , nodeCache = xformSubgraph igs.subgraph
    , nav = Navigation.singleton ( igs.startAt, initialFocus )
    , cam = cam
    , focus = getContent initialFocus
    }


simple : GameState -> ( GameState, List NodeId )
simple gs =
    ( gs, [] )


handleInput : UserInput -> GameState -> ( GameState, List NodeId )
handleInput uinp gs =
    case uinp of
        Pan dx dy ->
            let
                zoom =
                    gs.cam.zoom

                panBy =
                    ( dx / zoom, dy / zoom )
            in
            simple { gs | cam = stopAnimation <| moveCam panBy gs.cam }

        Zoom dz cursorPos ->
            simple { gs | cam = stopAnimation <| zoomAbout cursorPos -dz <| gs.cam }

        Action PlayerInput.Center ->
            simple { gs | cam = camToWorldOrigin gs.cam }

        Action (PlayerInput.Click pos) ->
            handleClick pos gs


handleClick : Vec2 -> GameState -> ( GameState, List NodeId )
handleClick pos gs =
    let
        nodePositionsWorld =
            layoutTree <| getTreeWithLoadingNodes gs.nav

        clickPosWorld =
            Camera.camPosToWorldPos gs.cam pos

        clickedNode =
            getClickedNode nodePositionsWorld clickPosWorld
    in
    case clickedNode of
        Nothing ->
            simple gs

        Just ( _, Fetching ) ->
            simple gs

        Just ( nid, Loaded n ) ->
            let
                ( updatedTree, stillNeedToFetch ) =
                    addNeighbors gs.nodeCache nid (getOutgoing n) gs.nav

                updatedUpdatedTree =
                    addInFlight nid stillNeedToFetch updatedTree

                newCam =
                    -- PERF: Cache this! We compute it here, then in the view function to render the tree!
                    getTreeWithLoadingNodes updatedUpdatedTree
                        |> layoutTree
                        |> List.filter (\p -> Tuple.first p.content == nid)
                        |> List.map .pos
                        |> List.head
                        |> Maybe.map (\p -> zoomInOn p gs.cam)
                        |> Maybe.withDefault gs.cam
            in
            ( { gs
                | focus = getContent n
                , nav = updatedUpdatedTree
                , cam = newCam
              }
            , stillNeedToFetch
            )


getClickedNode : List (WithPos a) -> Vec2 -> Maybe a
getClickedNode ns clickWorld =
    let
        clickCounts n =
            vDistSqare clickWorld n.pos < 400
    in
    List.filter clickCounts ns |> List.head |> Maybe.map .content


partitionHits : Subgraph -> List NodeId -> ( List ( NodeId, Node ), List NodeId )
partitionHits =
    partitionHitsHelper ( [], [] )


partitionHitsHelper :
    ( List ( NodeId, Node ), List NodeId )
    -> Subgraph
    -> List NodeId
    -> ( List ( NodeId, Node ), List NodeId )
partitionHitsHelper result cache lookupThese =
    case lookupThese of
        [] ->
            result

        nid :: rest ->
            case getNode cache nid of
                Nothing ->
                    partitionHitsHelper (Tuple.mapSecond (\r -> nid :: r) result) cache rest

                Just n ->
                    partitionHitsHelper (Tuple.mapFirst (\r -> ( nid, n ) :: r) result) cache rest


addNeighbors : Subgraph -> NodeId -> List NodeId -> NavTree -> ( NavTree, List NodeId )
addNeighbors cache parent cids nt =
    let
        ( hits, misses ) =
            partitionHits cache cids

        newTree =
            insertNeighborsAt nt parent hits
    in
    ( newTree, misses )


expandCache : Generated.BackendApi.Subgraph -> GameState -> GameState
expandCache sg gs =
    let
        newSg =
            xformSubgraph sg

        newNav =
            insertFetchResults newSg gs.nav
    in
    { gs
        | nodeCache = BackendWrapper.union newSg gs.nodeCache
        , nav = newNav
    }


camToWorldOrigin : Camera -> Camera
camToWorldOrigin =
    focusOn ( 0, 0 ) 1


zoomInOn : ( Float, Float ) -> Camera -> Camera
zoomInOn p =
    focusOn p 1


handleTick : Float -> GameState -> GameState
handleTick deltaMs gs =
    { gs | cam = tickCam deltaMs gs.cam }


handleInputs : List UserInput -> GameState -> ( GameState, List NodeId )
handleInputs uips g =
    foldl
        (\u ( x, is ) ->
            let
                ( newGame, newInputs ) =
                    handleInput u x
            in
            ( newGame, is ++ newInputs )
        )
        (simple g)
        uips


resizeCamera : ( Float, Float ) -> GameState -> GameState
resizeCamera s gs =
    let
        oldCam =
            gs.cam

        newCam =
            { oldCam | canvasSize = s }
    in
    { gs | cam = newCam }
