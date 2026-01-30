module GameState exposing (..)

import BackendWrapper exposing (Subgraph, getNode, getOutgoing, xformSubgraph)
import Camera exposing (Camera, Vec2, focusOn, moveCam, stopAnimation, tickCam, vDistSqare, zoomAbout)
import Generated.BackendApi exposing (InitialGameState, NodeId)
import List exposing (foldl)
import Navigation exposing (NavTree, getTree, insertNeighborsAt)
import PlayerInput exposing (UserInput(..))
import Tree exposing (WithPos, layoutTree)


type alias GameState =
    { startAt : NodeId
    , endAt : NodeId
    , graph : Subgraph
    , nav : NavTree
    , focus : NodeId
    , cam : Camera
    }


fromInitial : Camera -> InitialGameState -> GameState
fromInitial cam igs =
    { startAt = igs.startAt
    , endAt = igs.endAt
    , graph = xformSubgraph igs.subgraph
    , nav = Navigation.singleton igs.startAt
    , cam = cam
    , focus = igs.startAt
    }


handleInput : UserInput -> GameState -> GameState
handleInput uinp gs =
    case uinp of
        Pan dx dy ->
            let
                zoom =
                    gs.cam.zoom

                panBy =
                    ( dx / zoom, dy / zoom )
            in
            { gs | cam = stopAnimation <| moveCam panBy gs.cam }

        Zoom dz cursorPos ->
            { gs | cam = stopAnimation <| zoomAbout cursorPos -dz <| gs.cam }

        Action PlayerInput.Center ->
            { gs | cam = camToWorldOrigin gs.cam }

        Action (PlayerInput.Click pos) ->
            handleClick pos gs


handleClick : Vec2 -> GameState -> GameState
handleClick pos gs =
    let
        nodePositionsWorld =
            layoutTree <| getTree gs.nav

        clickPosWorld =
            Camera.camPosToWorldPos gs.cam pos

        clickedNode =
            getClickedNode nodePositionsWorld clickPosWorld
    in
    case clickedNode of
        Nothing ->
            gs

        Just nid ->
            { gs | nav = insertNeighborsAt gs.nav nid (getNeighbors nid gs.graph) }


getClickedNode : List (WithPos NodeId) -> Vec2 -> Maybe NodeId
getClickedNode ns clickWorld =
    let
        clickCounts n =
            vDistSqare clickWorld n.pos < 400
    in
    List.filter clickCounts ns |> List.head |> Maybe.map .content


getNeighbors : NodeId -> Subgraph -> List NodeId
getNeighbors nid sg =
    let
        node =
            getNode sg nid
    in
    case node of
        Nothing ->
            []

        Just n ->
            getOutgoing n


camToWorldOrigin : Camera -> Camera
camToWorldOrigin =
    focusOn ( 0, 0 ) 1


zoomInOn : ( Float, Float ) -> Camera -> Camera
zoomInOn p =
    focusOn p 1


handleTick : Float -> GameState -> GameState
handleTick deltaMs gs =
    { gs | cam = tickCam deltaMs gs.cam }


handleInputs : List UserInput -> GameState -> GameState
handleInputs uips g =
    foldl handleInput g uips


resizeCamera : ( Float, Float ) -> GameState -> GameState
resizeCamera s gs =
    let
        oldCam =
            gs.cam

        newCam =
            { oldCam | canvasSize = s }
    in
    { gs | cam = newCam }
