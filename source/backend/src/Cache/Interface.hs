{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Cache.Interface (
    GraphStore (..),
)
where

import Cache.NodeId (NodeId)
import Data.Kind
import Data.Set
import Domain.Model (EdgeInfo, Node, NodeContent (..))
import TePapa.Decode (TePapaReference)

class (Monad (StoreM g)) => GraphStore g where
    type StoreM g :: Type -> Type
    initStore :: TePapaReference -> NodeContent -> StoreM g g
    addNode :: g -> TePapaReference -> NodeContent -> StoreM g NodeId
    addEdge :: g -> TePapaReference -> TePapaReference -> EdgeInfo -> StoreM g ()
    getKeys :: g -> StoreM g (NodeId, Set NodeId)
    getNode :: g -> NodeId -> StoreM g Node
