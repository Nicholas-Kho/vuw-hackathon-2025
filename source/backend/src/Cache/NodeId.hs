module Cache.NodeId (NodeId, unNodeId, mkNodeId) where

import Data.Aeson
import Data.Hashable (Hashable)
import GHC.Generics

newtype NodeId = MkNodeId {unNodeId :: Int}
    deriving (Eq, Ord, Generic, FromJSON, ToJSON, Show, ToJSONKey)

instance Hashable NodeId

-- DO NOT use this unless you are implementing a graph cache!
-- Graph caches have the invariant that looking up a node ID never fails,
-- which is enforced only by returning IDs on creation. Making an arbitrary
-- node ID and looking it up will probably result in a crash.
mkNodeId :: Int -> NodeId
mkNodeId = MkNodeId
