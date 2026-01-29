module GameState exposing (..)

import BackendWrapper exposing (Subgraph, xformSubgraph)
import Camera exposing (Camera, camPosToWorldPos, focusOn, moveCam, stopAnimation, tickCam, zoomAbout)
import Generated.BackendApi exposing (InitialGameState, NodeId)
import List exposing (foldl)
import Navigation exposing (Nav, initNav)
import PlayerInput exposing (UserInput(..))


type alias GameState =
    { startAt : NodeId
    , endAt : NodeId
    , graph : Subgraph
    , nav : Nav
    , cam : Camera
    }


fromInitial : Camera -> InitialGameState -> GameState
fromInitial cam igs =
    { startAt = igs.startAt
    , endAt = igs.endAt
    , graph = xformSubgraph igs.subgraph
    , cam = cam
    , nav = initNav igs.startAt
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
            { gs | cam = zoomInOn (camPosToWorldPos gs.cam pos) gs.cam }


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
