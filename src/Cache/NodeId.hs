{-# LANGUAGE TemplateHaskell #-}

module Cache.NodeId (NodeId, mkNodeId) where

import Data.Aeson
import Data.Hashable (Hashable)
import qualified Data.Text as T
import GHC.Generics
import Servant.Elm

{-
We store these as text internally so that JSON and Elm don't treat these as numbers
and cause silent integer overflows. Elm Ints are signed 32 bit whereas Haskell ones
can be signed 64 bit, and JS uses doubles (-2^53 to 2^53 - 1) to store numbers.
-}
newtype NodeId = MkNodeId {content :: T.Text}
    deriving (Eq, Ord, Generic, Show, ToJSONKey, FromJSONKey)

instance Hashable NodeId

-- DO NOT use this unless you are implementing a graph cache!
-- Graph caches have the invariant that looking up a node ID never fails,
-- which is enforced only by returning IDs on creation. Making an arbitrary
-- node ID and looking it up will probably result in a crash.
mkNodeId :: Int -> NodeId
mkNodeId = MkNodeId . T.show

deriveBoth Servant.Elm.defaultOptions ''NodeId
