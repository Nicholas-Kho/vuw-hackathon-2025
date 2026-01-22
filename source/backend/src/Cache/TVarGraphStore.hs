{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Cache.TVarGraphStore (Graph)
where

import Cache.Interface
import Cache.NodeId (NodeId, mkNodeId)
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as M
import Data.Hashable (hash)
import qualified Data.Set as S
import Domain.Model
import TePapa.Decode (TePapaReference)

data Graph = Graph
    { nodes :: M.Map NodeId NodeContent
    , internalToExternal :: M.Map NodeId TePapaReference
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
        edgesFrom <- M.empty
        edgesTo <- M.empty
        let rootId = mkNodeId . hash $ conts
        M.insert rootId conts initialNodes
        M.insert rootId eid inToEx
        keySet <- newTVar (S.singleton rootId)
        pure $
            Graph
                { nodes = initialNodes
                , internalToExternal = inToEx
                , edgesFrom = edgesFrom
                , edgesTo = edgesTo
                , keys = keySet
                , rootKey = rootId
                }

    getKeys g = do
        keySet <- readTVar $ keys g
        pure (rootKey g, keySet)
