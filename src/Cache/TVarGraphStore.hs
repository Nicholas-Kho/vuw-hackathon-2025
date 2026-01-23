{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Cache.TVarGraphStore (Graph)
where

import Cache.Interface
import Cache.NodeId (NodeId, mkNodeId)
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as M
import Control.Monad (forM)
import Data.Hashable (hash)
import qualified Data.Map.Strict as D
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Domain.Model
import TePapa.Decode (TePapaReference)

data Graph = Graph
    -- lookups for nodes and internalToExternal must be total,
    -- as we only generate a NodeId each time addNode is called, which takes
    -- a NodeContent and TePapaReference.
    { nodes :: M.Map NodeId NodeContent
    , internalToExternal :: M.Map NodeId TePapaReference
    , -- lookups from this map may be partial.
      externalToInternal :: M.Map TePapaReference NodeId
    , -- Edges from KEY to VALUEs
      edgesFrom :: M.Map TePapaReference (S.Set (TePapaReference, EdgeInfo))
    , -- Edges to KEY from VALUEs
      edgesTo :: M.Map TePapaReference (S.Set (TePapaReference, EdgeInfo))
    , keys :: TVar (S.Set NodeId)
    , rootKey :: NodeId
    }

instance GraphStore Graph where
    type StoreM Graph = STM
    initStore eid conts = do
        initialNodes <- M.empty
        inToEx <- M.empty
        exToIn <- M.empty
        edgesFrom <- M.empty
        edgesTo <- M.empty
        let rootId = mkNodeId . hash $ conts
        M.insert rootId conts initialNodes
        M.insert rootId eid inToEx
        M.insert eid rootId exToIn
        keySet <- newTVar (S.singleton rootId)
        pure $
            Graph
                { nodes = initialNodes
                , internalToExternal = inToEx
                , externalToInternal = exToIn
                , edgesFrom = edgesFrom
                , edgesTo = edgesTo
                , keys = keySet
                , rootKey = rootId
                }

    getKeys g = do
        keySet <- readTVar $ keys g
        pure (rootKey g, keySet)

    addNode g eid content = do
        -- Invariants assumed by addNode:
        --
        -- 1. Hash collisions are negligibly unlikely.
        --
        -- 2. Each TePapa object maps to exactly one external ID.
        --    (This function is not called with identical content but differing external IDs.)
        --
        -- 3. Each TePapa external ID maps to exactly one object.
        --    (This function is not called with identical external IDs but differing content.)
        --
        -- Desired property:
        --   Calling addNode multiple times with identical content and external ID
        --   should return the same NodeId.
        --
        -- For prototyping, these invariants are assumed but not enforced.
        -- TODO: Detect and handle duplicate content and/or external IDs.
        let nid = mkNodeId . hash $ content
        M.insert nid eid (internalToExternal g)
        M.insert eid nid (externalToInternal g)
        M.insert nid content (nodes g)
        modifyTVar' (keys g) (S.insert nid)
        pure nid

    getNode g nid = do
        nodeContent <-
            M.lookup nid (nodes g) >>= \case
                Just nc -> pure nc
                Nothing -> error "Invarint violated: node ID not in store.nodes!"
        externalId <- getExternal g nid
        -- find and build edges
        -- We have to convert to a list here because sets don't have a mapM.
        outList <- M.lookup externalId (edgesFrom g) >>= pure . S.elems . fromMaybe S.empty
        outNidList <- mapMaybe id <$> forM outList (\(eid, reason) -> M.lookup eid (externalToInternal g) >>= pure . fmap (,reason))
        let outEdges = foldl' (\outMap (toNid, info) -> D.insertWith (S.union) toNid (S.singleton info) outMap) D.empty outNidList
        inList <- M.lookup externalId (edgesTo g) >>= pure . S.elems . fromMaybe S.empty
        inNidList <- mapMaybe id <$> forM inList (\(eid, reason) -> M.lookup eid (externalToInternal g) >>= pure . fmap (,reason))
        let inEdges = foldl' (\inMap (fromNid, info) -> D.insertWith (S.union) fromNid (S.singleton info) inMap) D.empty inNidList
        pure $
            Node
                { outgoingEdges = outEdges
                , content = nodeContent
                , incomingEdges = inEdges
                }

    addEdge g from to info = do
        oldFSet <- M.lookup from (edgesFrom g) >>= pure . fromMaybe S.empty
        let newFSet = S.insert (to, info) oldFSet
        M.insert from newFSet (edgesFrom g)
        oldTSet <- M.lookup to (edgesTo g) >>= pure . fromMaybe S.empty
        let newTSet = S.insert (from, info) oldTSet
        M.insert to newTSet (edgesTo g)

    getExternal g nid = do
        M.lookup nid (internalToExternal g) >>= \case
            Just nc -> pure nc
            Nothing -> error "Invarint violated: node ID not in store.internalToExternal!"
