module GameState exposing (..)

import Camera exposing (Camera, mkCamera)
import Generated.BackendApi exposing (InitialGameState, NodeId, Subgraph)


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
