module Navigation exposing
    ( NavTree
    , getTree
    , singleton
    , toNodeTree
    )

import BackendWrapper exposing (Node, Subgraph, getNode, unwrapNodeId)
import Dict exposing (Dict)
import Generated.BackendApi exposing (NodeId)
import Set exposing (Set)
import Tree exposing (Tree(..))


type NavTree
    = NavTree
        { tree : Tree NodeId

        -- Careful: These strings are unwrapped NodeIds.
        , members : Set String
        , loopsTo : Dict String (Set String)
        }


getTree : NavTree -> Tree NodeId
getTree (NavTree nt) =
    nt.tree


singleton : NodeId -> NavTree
singleton nid =
    NavTree
        { tree = Node nid []
        , members = Set.singleton <| unwrapNodeId nid
        , loopsTo = Dict.empty
        }


toNodeTree : Subgraph -> NavTree -> Tree (Maybe Node)
toNodeTree sg (NavTree nt) =
    Tree.map (getNode sg) nt.tree
