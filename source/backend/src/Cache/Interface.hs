{-# LANGUAGE TypeFamilies #-}

module Cache.Interface (
    FetchResult (..),
    GraphStore (..),
)
where

import Data.Kind
import Data.Set
import Domain.Model (Edge, Node (..), NodeContent, NodeId)
import Servant.Client (ClientError)

data FetchResult
    = AlreadyThere (Either ClientError NodeContent)
    | WaitForResult
    | Proceed

class (Monad (StoreM g)) => GraphStore g where
    type StoreM g :: Type -> Type
    blankGraph :: StoreM g g
    readNode :: g -> NodeId -> StoreM g (Maybe Node)
    writeNode :: g -> NodeId -> Node -> StoreM g ()
    deleteNode :: g -> NodeId -> StoreM g (Maybe Node)
    outgoingEdges :: g -> NodeId -> StoreM g (Maybe (Set Edge))
    link :: g -> Edge -> StoreM g ()

    tryFetch :: g -> NodeId -> StoreM g FetchResult
    tryFetch graph nid = do
        readNode graph nid >>= \case
            Nothing -> pure Proceed
            Just NotFetched -> pure Proceed
            Just Fetching -> pure WaitForResult
            Just (Fail ce) -> pure . AlreadyThere . Left $ ce
            Just (Ok nc) -> pure . AlreadyThere . Right $ nc

    commitNode :: g -> NodeId -> NodeContent -> StoreM g ()
    commitNode graph nid content = writeNode graph nid (Ok content)

    failNode :: g -> NodeId -> ClientError -> StoreM g ()
    failNode graph nid cerr = writeNode graph nid (Fail cerr)
