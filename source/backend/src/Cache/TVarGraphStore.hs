{-# LANGUAGE TypeFamilies #-}

module Cache.TVarGraphStore (Graph, sweepDeleted)
where

import Cache.Interface
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as M
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Set as S
import Data.Text
import Domain.Model

type PartEdge = (NodeId, Text)

partEdgeFrom :: NodeId -> PartEdge -> Edge
partEdgeFrom nidFrom (nidTo, txt) = Edge{from = nidFrom, to = nidTo, info = txt}

partEdgeTo :: NodeId -> PartEdge -> Edge
partEdgeTo nidTo (nidFrom, txt) = Edge{from = nidFrom, to = nidTo, info = txt}

data Graph = Graph
    { nodes :: M.Map NodeId Node
    , edgesFrom :: M.Map NodeId (S.Set PartEdge)
    , edgesTo :: M.Map NodeId (S.Set PartEdge)
    , toDelete :: TVar (S.Set NodeId)
    , isLocked :: TVar Bool
    }

insertPartEdge :: NodeId -> PartEdge -> M.Map NodeId (S.Set PartEdge) -> STM ()
insertPartEdge nid pe emap = do
    edgeSet <-
        M.lookup nid emap >>= \case
            Nothing -> pure $ S.singleton pe
            Just s -> pure $ S.insert pe s
    M.insert nid edgeSet emap

waitForLock :: Graph -> STM ()
waitForLock graph = do
    locked <- readTVar (isLocked graph)
    check (not locked)

instance GraphStore Graph where
    type StoreM Graph = STM
    blankGraph =
        Graph
            <$> M.empty
            <*> M.empty
            <*> M.empty
            <*> newTVar S.empty
            <*> newTVar False
    readNode graph nid =
        waitForLock graph >> M.lookup nid (nodes graph)
    failNode graph nid cerr =
        waitForLock graph >> M.insert nid (Fail cerr) (nodes graph)
    outgoingEdges graph nid =
        waitForLock graph >> M.lookup nid (edgesFrom graph) >>= \case
            Nothing -> pure S.empty
            Just partialEdgeSet -> pure $ S.map (partEdgeFrom nid) partialEdgeSet
    commitNode graph nid nconts =
        waitForLock graph >> M.insert nid (Ok nconts) (nodes graph)
    tryFetch graph nid =
        waitForLock graph >> M.lookup nid (nodes graph) >>= \case
            Nothing -> pure Proceed
            Just NotFetched -> pure Proceed
            Just Fetching -> pure WaitForResult
            Just (Fail cerr) -> pure . AlreadyThere . Left $ cerr
            Just (Ok conts) -> pure . AlreadyThere . Right $ conts
    deleteNode graph nid = do
        node <- readNode graph nid
        M.delete nid (nodes graph)
        M.delete nid (edgesFrom graph)
        M.delete nid (edgesTo graph)
        modifyTVar' (toDelete graph) (S.insert nid)
        pure node
    link graph edge = do
        waitForLock graph
        insertPartEdge (from edge) (to edge, info edge) (edgesFrom graph)
        insertPartEdge (to edge) (from edge, info edge) (edgesTo graph)

-- The delete function in the interface just marks stuff for deletion.
-- To actually free the memory, we need to call this.
sweepDeleted :: (MonadIO m) => Graph -> m ()
sweepDeleted graph = do
    toDelete <- liftIO . atomically $ do
        writeTVar (isLocked graph) True
        s <- readTVar (toDelete graph)
        writeTVar (toDelete graph) (S.empty)
        pure s
    forM_
        toDelete
        ( \k -> liftIO $ do
            M.unsafeDelete k (nodes graph)
            M.unsafeDelete k (edgesFrom graph)
            M.unsafeDelete k (edgesTo graph)
        )
