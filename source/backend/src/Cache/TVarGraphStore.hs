{-# LANGUAGE TypeFamilies #-}

module Cache.TVarGraphStore (Graph, sweepDeleted)
where

import Cache.Interface
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as M
import Control.Exception
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Set as S
import Domain.Model

data Graph = Graph
    { nodes :: M.Map NodeId (TMVar Node)
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
    outgoingEdges graph nid =
        waitForLock graph >> M.lookup nid (edgesFrom graph) >>= \case
            Nothing -> pure Nothing
            Just partialEdgeSet -> pure . Just $ S.map (partEdgeFrom nid) partialEdgeSet
    link graph edge = do
        waitForLock graph
        insertPartEdge (from edge) (to edge, info edge) (edgesFrom graph)
        insertPartEdge (to edge) (from edge, info edge) (edgesTo graph)
    readNode g nid = do
        waitForLock g
        M.lookup nid (nodes g) >>= \case
            Nothing -> pure Missing
            Just tmHandle ->
                tryReadTMVar tmHandle >>= \case
                    Nothing -> pure (NeedToWait tmHandle)
                    Just n -> pure (ItWasThere n)
    claimFetch g nid = do
        waitForLock g
        M.lookup nid (nodes g) >>= \case
            Just tmHandle ->
                tryReadTMVar tmHandle >>= \case
                    Nothing -> pure (WaitForResult tmHandle)
                    Just n -> pure (AlreadyThere n)
            Nothing -> do
                obligation <- newEmptyTMVar
                M.insert nid obligation (nodes g)
                pure (Proceed obligation)
    deleteNode g nid = do
        waitForLock g
        modifyTVar' (toDelete g) (S.insert nid)
        res <-
            M.lookup nid (nodes g) >>= \case
                Nothing -> pure Missing
                Just tmHandle ->
                    tryReadTMVar tmHandle >>= \case
                        Nothing -> pure (NeedToWait tmHandle)
                        Just n -> pure (ItWasThere n)

        M.delete nid (nodes g)
        pure res

-- The delete function in the interface just marks stuff for deletion.
-- To actually free the memory, we need to call this.
sweepDeleted :: (MonadIO m) => Graph -> m ()
sweepDeleted graph = do
    toDelete <- liftIO . atomically $ do
        writeTVar (isLocked graph) True
        s <- readTVar (toDelete graph)
        writeTVar (toDelete graph) (S.empty)
        pure s
    liftIO $
        flip finally (atomically $ writeTVar (isLocked graph) False) $
            forM_ toDelete $ \k -> do
                M.unsafeDelete k (nodes graph)
                M.unsafeDelete k (edgesFrom graph)
                M.unsafeDelete k (edgesTo graph)
