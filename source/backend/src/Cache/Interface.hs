{-# LANGUAGE TypeFamilies #-}

module Cache.Interface (
    FetchResult (..),
    GraphStore (..),
)
where

import Data.Kind
import Data.Set
import Domain.Model (Edge, Node, NodeContent, NodeId)
import Servant.Client (ClientError)

data FetchResult
    = AlreadyThere (Either ClientError NodeContent)
    | WaitForResult
    | Proceed

class (Monad (StoreM g)) => GraphStore g where
    type StoreM g :: Type -> Type
    blankGraph :: StoreM g g
    readNode :: g -> NodeId -> StoreM g (Maybe Node)
    tryFetch :: g -> NodeId -> StoreM g FetchResult
    deleteNode :: g -> NodeId -> StoreM g (Maybe Node)
    commitNode :: g -> NodeId -> NodeContent -> StoreM g ()
    failNode :: g -> NodeId -> ClientError -> StoreM g ()
    outgoingEdges :: g -> NodeId -> StoreM g (Maybe (Set Edge))
    link :: g -> Edge -> StoreM g ()
