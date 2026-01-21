{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Cache.TVarGraphStore (Graph, showCache)
where

import Cache.Interface
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as M
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
    , root :: (NodeId, Node)
    , isLocked :: TVar Bool
    }

waitForLock :: Graph -> STM ()
waitForLock graph = do
    locked <- readTVar (isLocked graph)
    check (not locked)

instance GraphStore Graph where
    type StoreM Graph = STM
    initGraph rt =
        Graph
            <$> M.empty
            <*> M.empty
            <*> M.empty
            <*> newTVar S.empty
            <*> pure rt
            <*> newTVar False
    listKeys = readTVar . keys
    getRoot graph = pure $ root graph
    outgoingEdges graph nid =
        waitForLock graph >> M.lookup nid (edgesFrom graph) >>= \case
            Nothing -> pure Nothing
            Just partialEdgeSet -> pure . Just $ S.map (mkEdgeFrom nid) partialEdgeSet

    readNode g nid = do
        waitForLock g
        M.lookup nid (nodes g) >>= \case
            Nothing -> pure Missing
            Just tmHandle ->
                tryReadTMVar tmHandle >>= \case
                    Nothing -> pure (NeedToWait tmHandle)
                    Just n -> pure (ItWasThere n)

    runAction g action = do
        waitForLock g
        case action of
            AddNode nid n ->
                M.lookup nid (nodes g) >>= \case
                    Nothing -> newTMVar n >>= \mv -> M.insert nid mv (nodes g)
                    -- This means the node is cached or another thread is writing,
                    -- so ignore the operation
                    Just _ -> pure ()
            AddEdge edge -> do
                -- Set PartEdgeFrom
                esTo <- fromMaybe S.empty <$> M.lookup (edge.to) (g.edgesTo)
                -- Set PartEdgeTo
                esFrom <- fromMaybe S.empty <$> M.lookup (edge.from) (g.edgesFrom)
                let partTo = PartEdgeTo{to = edge.to, info = edge.info}
                let partFrom = PartEdgeFrom{from = edge.from, info = edge.info}
                M.insert edge.from (S.insert partTo esFrom) g.edgesFrom
                M.insert edge.to (S.insert partFrom esTo) g.edgesTo

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
