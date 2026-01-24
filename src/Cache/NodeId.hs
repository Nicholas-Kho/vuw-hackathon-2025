module Cache.NodeId (NodeId, unNodeId, mkNodeId) where

import Data.Aeson
import Data.Hashable (Hashable)
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readMaybe)

newtype NodeId = MkNodeId {unNodeId :: Int}
    deriving (Eq, Ord, Generic, Show, ToJSONKey)

instance Hashable NodeId

-- When we send node IDs to javascript, we convert them to strings
-- because JS numbers are weird and will cause problems

instance ToJSON NodeId where
    toJSON MkNodeId{unNodeId = x} = String . T.show $ x

instance FromJSON NodeId where
    parseJSON = withText "nodeId" $ \nidt ->
        case readMaybe (T.unpack nidt) of
            Nothing -> fail $ "Invalid NodeId " <> (T.unpack nidt)
            Just x -> pure . mkNodeId $ x

-- DO NOT use this unless you are implementing a graph cache!
-- Graph caches have the invariant that looking up a node ID never fails,
-- which is enforced only by returning IDs on creation. Making an arbitrary
-- node ID and looking it up will probably result in a crash.
mkNodeId :: Int -> NodeId
mkNodeId = MkNodeId
