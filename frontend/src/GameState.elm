module GameState exposing (..)

import Generated.BackendApi exposing (InitialGameState, NodeId, Subgraph)
import Math.Vector2 as W
import Math.Vector3 as WV


type alias GameState =
    { startAt : NodeId
    , endAt : NodeId
    , graph : Subgraph
    , cam : Camera
    }


type alias Camera =
    { pos : W.Vec Float
    , basis : Mat2
    }


type alias Mat2 =
    { row1 : W.Vec Float
    , row2 : W.Vec Float
    }


type alias Mat3 =
    { row1 : WV.Vec Float
    , row2 : WV.Vec Float
    , row3 : WV.Vec Float
    }


mid2 : Mat2
mid2 =
    { row1 = W.vec2 1 0
    , row2 = W.vec2 0 1
    }


mkCamera : Camera
mkCamera =
    { pos = W.vec2 0 0
    , basis = mid2
    }


fromInitial : InitialGameState -> GameState
fromInitial igs =
    { startAt = igs.startAt
    , endAt = igs.endAt
    , graph = igs.subgraph
    , cam = mkCamera
    }
