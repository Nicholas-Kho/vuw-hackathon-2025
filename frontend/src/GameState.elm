module GameState exposing (..)

import Camera exposing (Camera, moveCam, setAnimation, tickCam, zoomAbout)
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
            { gs | cam = moveCam ( dx / gs.cam.zoom, dy / gs.cam.zoom ) gs.cam }

        Zoom dz cp ->
            { gs | cam = zoomAbout gs.cam cp -dz }

        Action PlayerInput.Center ->
            { gs | cam = setAnimation (snapToOrigin gs.cam) gs.cam }


snapToOrigin : Camera -> Camera.Animation
snapToOrigin cam =
    { start = cam.worldPos
    , end = ( 0, 0 )
    , timeElapsed = 0
    , duration = 200
    }


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
