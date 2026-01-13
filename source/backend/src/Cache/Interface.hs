module Cache.Interface (
    FetchResult (..),
    GraphStore (..),
)
where

import Data.Set
import Domain.Model (Edge, Node, NodeContent, NodeId)
import Servant.Client (ClientError)

data FetchResult
    = AlreadyThere (Either ClientError NodeContent)
    | WaitForResult
    | Proceed

class (Monad m) => GraphStore g m where
    readNode :: g -> NodeId -> m Node
    tryFetch :: g -> NodeId -> m FetchResult
    deleteNode :: g -> NodeId -> m Node
    commitNode :: g -> NodeId -> NodeContent -> m ()
    failNode :: g -> NodeId -> ClientError -> m ()
    outgoingEdges :: g -> NodeId -> m (Set Edge)
    link :: g -> Edge -> m ()
