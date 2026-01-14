{-# LANGUAGE TypeFamilies #-}

module Cache.Interface (
    FetchResult (..),
    GraphStore (..),
    MonadWait (..),
)
where

import Control.Monad (forM_)
import Data.Kind
import Data.Set
import Domain.Model (Edge (..), GraphFragment (..), Node (..), NodeContent, NodeId, partEdgeFrom, partEdgeTo)
import MonadWait
import Servant.Client (ClientError)

data FetchResult w
    = AlreadyThere (Either ClientError NodeContent)
    | WaitForResult (w (Either ClientError NodeContent))
    | Proceed

class (Monad (StoreM g)) => GraphStore g where
    type StoreM g :: Type -> Type
    blankGraph :: StoreM g g
    readNode :: g -> NodeId -> StoreM g (Maybe Node)
    writeNode :: g -> NodeId -> Node -> StoreM g ()
    deleteNode :: g -> NodeId -> StoreM g (Maybe Node)
    outgoingEdges :: g -> NodeId -> StoreM g (Maybe (Set Edge))
    link :: g -> Edge -> StoreM g ()
    tryFetch :: g -> NodeId -> StoreM g (FetchResult (Wait (StoreM g)))

    commitNode :: g -> NodeId -> NodeContent -> StoreM g ()
    commitNode graph nid content = writeNode graph nid (Ok content)

    failNode :: g -> NodeId -> ClientError -> StoreM g ()
    failNode graph nid cerr = writeNode graph nid (Fail cerr)

    stitchFragment :: g -> NodeId -> GraphFragment -> StoreM g ()
    stitchFragment g nid frag = do
        commitNode g nid (content frag)
        forM_ (outEdges frag) (link g . partEdgeFrom nid)
        forM_ (inEdges frag) (link g . partEdgeTo nid)
