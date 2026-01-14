{-# LANGUAGE TypeFamilies #-}

module Cache.Interface (
    FetchResult (..),
    GraphStore (..),
    MonadWait (..),
    ReadResult (..),
)
where

import Data.Kind
import Data.Set
import Domain.Model (Edge (..), Node (..), NodeId)
import MonadFulfil
import MonadWait

data FetchResult w o
    = AlreadyThere Node
    | WaitForResult (w Node)
    | Proceed o

data ReadResult w
    = ItWasThere Node
    | NeedToWait (w Node)
    | Missing

type NodeObligation g = Fulfil (StoreM g) Node

class (Monad (StoreM g)) => GraphStore g where
    type StoreM g :: Type -> Type
    outgoingEdges :: g -> NodeId -> StoreM g (Maybe (Set Edge))
    link :: g -> Edge -> StoreM g ()
    blankGraph :: StoreM g g
    readNode :: g -> NodeId -> StoreM g (ReadResult (Wait (StoreM g)))
    deleteNode :: g -> NodeId -> StoreM g (ReadResult (Wait (StoreM g)))
    claimFetch :: g -> NodeId -> StoreM g (FetchResult (Wait (StoreM g)) (NodeObligation g))
