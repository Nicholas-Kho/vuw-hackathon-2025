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
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Domain.Model
import TePapa.ExternalId (TePapaReference)

data Graph = Graph
    -- lookups for nodes and internalToExternal must be total,
    -- as we only generate a NodeId each time addNode is called, which takes
    -- a NodeContent and TePapaReference.
    { nodes :: M.Map NodeId NodeContent
    , internalToExternal :: M.Map NodeId TePapaReference
    , -- lookups from this map may be partial.
      externalToInternal :: M.Map TePapaReference NodeId
    , -- Edges from KEY to VALUEs
      edgesFrom :: M.Map TePapaReference (D.Map TePapaReference EdgeInfo)
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
        M.insert nid eid g.internalToExternal
        M.insert eid nid g.externalToInternal
        M.insert nid content g.nodes
        modifyTVar' g.keys (S.insert nid)
        pure nid

    getNode g nid = do
        nodeContent <-
            M.lookup nid (g.nodes) >>= \case
                Just nc -> pure nc
                Nothing -> error "Invarint violated: node ID not in store.nodes!"
        externalId <- getExternal g nid
        outMap <- fromMaybe D.empty <$> M.lookup externalId g.edgesFrom
        presentEdges <- fmap catMaybes $ forM (D.assocs outMap) $ \(toEid, edgeReason) ->
            fmap (,edgeReason) <$> M.lookup toEid g.externalToInternal
        pure $
            Node
                { outgoingEdges = D.fromList presentEdges
                , content = nodeContent
                , nodeId = nid
                }

    addEdge g from to info =
        M.lookup from (g.edgesFrom) >>= \case
            Just outgoingMap ->
                M.insert from (D.insertWith (<>) to info outgoingMap) g.edgesFrom
            Nothing ->
                M.insert from (D.singleton to info) g.edgesFrom

    getExternal g nid = do
        M.lookup nid g.internalToExternal >>= \case
            Just nc -> pure nc
            Nothing -> error "Invarint violated: node ID not in store.internalToExternal!"
