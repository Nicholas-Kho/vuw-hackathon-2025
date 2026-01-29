module GameState exposing (..)

import Camera exposing (Camera, focusOn, moveCam, stopAnimation, tickCam, zoomAbout)
import Generated.BackendApi exposing (InitialGameState, NodeId, Subgraph)
import List exposing (foldl)
import PlayerInput exposing (UserInput(..))


type alias GameState =
    { startAt : NodeId
    , endAt : NodeId
    , graph : Subgraph
    , cam : Camera
    }


fromInitial : Camera -> InitialGameState -> GameState
fromInitial cam igs =
    { startAt = igs.startAt
    , endAt = igs.endAt
    , graph = igs.subgraph
    , cam = cam
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

        Action _ ->
            { gs | cam = camToWorldOrigin gs.cam }


camToWorldOrigin : Camera -> Camera
camToWorldOrigin =
    focusOn ( 0, 0 ) 1


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
