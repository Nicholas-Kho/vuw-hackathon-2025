module GameState exposing (..)

import Camera exposing (Camera, mkCamera, moveCam, zoomCam)
import Generated.BackendApi exposing (InitialGameState, NodeId, Subgraph)
import List exposing (foldl)
import PlayerInput exposing (UserInput(..))


type alias GameState =
    { startAt : NodeId
    , endAt : NodeId
    , graph : Subgraph
    , cam : Camera
    }


fromInitial : InitialGameState -> GameState
fromInitial igs =
    { startAt = igs.startAt
    , endAt = igs.endAt
    , graph = igs.subgraph
    , cam = mkCamera
    }


handleInput : UserInput -> GameState -> GameState
handleInput uinp gs =
    case uinp of
        Pan dx dy ->
            { gs | cam = moveCam ( dx, dy ) gs.cam }

        Zoom dz ->
            { gs | cam = zoomCam dz gs.cam }


handleInputs : List UserInput -> GameState -> GameState
handleInputs uips g =
    foldl handleInput g uips
