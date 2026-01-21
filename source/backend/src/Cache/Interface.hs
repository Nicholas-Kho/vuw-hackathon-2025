{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Cache.Interface (
    FetchResult (..),
    GraphAction (..),
    GraphStore (..),
    MonadWait (..),
    ReadResult (..),
    graphActionPrettyPrint,
)
where

import Data.Aeson (ToJSON)
import Data.Kind
import Data.Set
import qualified Data.Text as T
import Domain.Model (Edge (..), Node (..), NodeId, prettyPrintNode, prettyPrintNodeId)
import GHC.Generics (Generic)
import MonadWait

data GraphAction
    = AddNode NodeId Node
    | AddEdge Edge
    deriving (Show, Generic, ToJSON)

graphActionPrettyPrint :: GraphAction -> IO ()
graphActionPrettyPrint (AddNode nid n) =
    putStrLn $ prettyPrintNodeId nid <> " ::= " <> prettyPrintNode n
graphActionPrettyPrint (AddEdge Edge{from = fid, to = tid, info = inf}) =
    putStrLn $
        prettyPrintNodeId fid
            <> " -> "
            <> prettyPrintNodeId tid
            <> " because "
            <> T.unpack inf

data FetchResult w o
    = AlreadyThere Node
    | WaitForResult (w Node)
    | Proceed o

data ReadResult w
    = ItWasThere Node
    | NeedToWait (w Node)
    | Missing

class (Monad (StoreM g)) => GraphStore g where
    type StoreM g :: Type -> Type
    runAction :: g -> GraphAction -> StoreM g ()
    outgoingEdges :: g -> NodeId -> StoreM g (Maybe (Set Edge))
    initGraph :: (NodeId, Node) -> StoreM g g
    readNode :: g -> NodeId -> StoreM g (ReadResult (Wait (StoreM g)))
    listKeys :: g -> StoreM g (Set NodeId)
    getRoot :: g -> StoreM g (NodeId, Node)
