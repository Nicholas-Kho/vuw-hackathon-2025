module Navigation exposing (..)

import BackendWrapper exposing (Node, Subgraph, getNode)
import Generated.BackendApi exposing (NodeId)
import Tree exposing (Tree(..))


type alias NavTree =
    Tree NodeId


singleton : NodeId -> NavTree
singleton nid =
    Node nid []


toNodeTree : Subgraph -> NavTree -> Tree (Maybe Node)
toNodeTree sg =
    Tree.map (getNode sg)
