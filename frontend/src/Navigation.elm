module Navigation exposing
    ( Nav
    , initNav
    )

import Generated.BackendApi exposing (NodeId)
import Set exposing (Set)


type alias Nav =
    { selected : NodeId
    , history : List NodeId
    , expanded : Set NodeId
    }


initNav : NodeId -> Nav
initNav from =
    { selected = from
    , history = []
    , expanded = Set.empty
    }
