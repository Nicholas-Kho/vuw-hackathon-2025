{-# LANGUAGE TypeFamilies #-}

module Cache.Interface (
    FetchResult (..),
    GraphStore (..),
    MonadWait (..),
    ReadResult (..),
    runAction,
)
where

import Data.Kind
import Data.Set
import Domain.Model (Edge (..), Node (..), NodeId)
import MonadFulfil
import MonadWait
import TePapa.Convert (GraphAction (..))

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
    listKeys :: g -> StoreM g (Set NodeId)

runAction ::
    ( GraphStore g
    , MonadFulfil (StoreM g)
    ) =>
    g ->
    GraphAction ->
    StoreM g ()
runAction g action = do
    case action of
        AddNode nid n -> do
            claimFetch g nid >>= \case
                -- Fetching a node which is already there, ignore the op.
                AlreadyThere _ -> pure ()
                -- Someone else is already getting it, ignore the op.
                WaitForResult _ -> pure ()
                Proceed obligation -> fulfil obligation n
        AddEdge e -> link g e
