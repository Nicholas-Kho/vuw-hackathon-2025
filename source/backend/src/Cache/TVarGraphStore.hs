{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Cache.TVarGraphStore (Graph, sweepDeleted, showCache)
where

import Cache.Interface
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as M
import Control.Exception
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (unpack)
import Domain.Model

data Graph = Graph
    { nodes :: M.Map NodeId (TMVar Node)
    , -- Edges from KEY to VALUES
      edgesFrom :: M.Map NodeId (S.Set PartEdgeTo)
    , -- Edges to KEY from VALUES
      edgesTo :: M.Map NodeId (S.Set PartEdgeFrom)
    , keys :: TVar (S.Set NodeId)
    , toDelete :: TVar (S.Set NodeId)
    , isLocked :: TVar Bool
    }

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
            <*> newTVar S.empty
            <*> newTVar False
    listKeys = readTVar . keys
    outgoingEdges graph nid =
        waitForLock graph >> M.lookup nid (edgesFrom graph) >>= \case
            Nothing -> pure Nothing
            Just partialEdgeSet -> pure . Just $ S.map (mkEdgeFrom nid) partialEdgeSet
    link graph edge = do
        waitForLock graph
        -- Set PartEdgeFrom
        esTo <- fromMaybe S.empty <$> M.lookup (edge.to) (graph.edgesTo)
        -- Set PartEdgeTo
        esFrom <- fromMaybe S.empty <$> M.lookup (edge.from) (graph.edgesFrom)
        let partTo = PartEdgeTo{to = edge.to, info = edge.info}
        let partFrom = PartEdgeFrom{from = edge.from, info = edge.info}
        M.insert edge.from (S.insert partTo esFrom) graph.edgesFrom
        M.insert edge.to (S.insert partFrom esTo) graph.edgesTo

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
                modifyTVar' (keys g) (S.insert nid)
                pure (Proceed obligation)
    deleteNode g nid = do
        waitForLock g
        modifyTVar' (toDelete g) (S.insert nid)
        modifyTVar' (keys g) (S.delete nid)
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

showCache :: (MonadIO m) => Graph -> m ()
showCache g = do
    -- We need to block the store for a bit so we can get a consistent snapshot
    liftIO . atomically $ writeTVar (isLocked g) True
    nodePromiseList <- liftIO $ M.unsafeToList (nodes g)
    partEdgesFrom <- liftIO $ M.unsafeToList (edgesFrom g)
    partEdgesTo <- liftIO $ M.unsafeToList (edgesTo g)
    liftIO . atomically $ writeTVar (isLocked g) False
    -- It's possible other threads were writing when we got the nodeList, so
    -- we need to finish waiting for those so we can print them.
    nodeList <- forM nodePromiseList $ \(nid, promise) -> do
        content <- liftIO . atomically . readTMVar $ promise
        pure (nid, content)
    -- convert our [(nodeId, Set PartEdge)] to [Edge]
    let fullEdgesA = foldl' S.union S.empty . map (\(fromId, pet) -> S.map (mkEdgeFrom fromId) pet) $ partEdgesFrom
    let fullEdgesB = foldl' S.union S.empty . map (\(toId, pef) -> S.map (mkEdgeTo toId) pef) $ partEdgesTo
    let allEdges = fullEdgesA <> fullEdgesB
    liftIO . putStrLn $ "Nodes:"
    liftIO . forM_ nodeList $
        \(nid, node) -> putStrLn (prettyPrintNodeId nid <> " is " <> show node) >> putStrLn ""
    liftIO . putStrLn $ "Edges:"
    liftIO . forM_ allEdges $ \e ->
        putStrLn (prettyPrintNodeId (e.from) <> " -> " <> prettyPrintNodeId (e.to) <> "(" <> (unpack e.info) <> ")")
