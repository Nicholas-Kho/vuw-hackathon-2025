{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Model (
    Edge (..),
    Node (..),
    NodeContent (..),
    NodeId (..),
    NodeType (..),
    PartEdgeFrom (..),
    PartEdgeTo (..),
    mkEdgeFrom,
    mkEdgeTo,
    nodeIdToExternal,
    prettyPrintNode,
    prettyPrintNodeId,
)
where

import Data.Aeson
import Data.Hashable
import Data.Text
import GHC.Generics
import Servant.Client (ClientError)
import TePapa.Decode (ExternalId (..), MuseumResource (..), TePapaReference (..))

data NodeType
    = ObjectN
    | AgentN
    | PlaceN
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance Hashable NodeType

newtype NodeId = NodeId {unNodeId :: (NodeType, Int)}
    deriving (Eq, Ord, Show, Generic)

instance Hashable NodeId

prettyPrintNodeId :: NodeId -> String
prettyPrintNodeId NodeId{unNodeId = (nt, x)} = (Prelude.show nt) <> "/" <> (Prelude.show x)

nodeIdToExternal :: NodeId -> TePapaReference
nodeIdToExternal NodeId{unNodeId = (nt, x)} =
    case nt of
        ObjectN -> TePapaReference{namespace = ObjectR, eid = ExternalId x}
        AgentN -> TePapaReference{namespace = AgentR, eid = ExternalId x}
        PlaceN -> TePapaReference{namespace = PlaceR, eid = ExternalId x}

data NodeContent = NodeContent
    { title :: Text
    , description :: Text
    , thumbnailUrl :: Maybe Text
    }
    deriving (Show, Generic, ToJSON)

data Node = Fail ClientError | Ok NodeContent deriving (Show)

prettyPrintNode :: Node -> String
prettyPrintNode (Fail cerr) = "FAIL: " <> (Prelude.show cerr)
prettyPrintNode (Ok nc) =
    "OK: "
        <> (unpack . title $ nc)
        <> ": "
        <> (Prelude.take 15 . unpack . description $ nc)
        <> (if (Data.Text.length . description $ nc) > 15 then "..." else "")

data Edge = Edge
    { from :: NodeId
    , to :: NodeId
    , info :: Text
    }
    deriving (Eq, Ord, Show, Generic, ToJSON)

data PartEdgeTo = PartEdgeTo
    { to :: NodeId
    , info :: Text
    }
    deriving (Eq, Ord)

data PartEdgeFrom = PartEdgeFrom
    { from :: NodeId
    , info :: Text
    }
    deriving (Eq, Ord)

mkEdgeFrom :: NodeId -> PartEdgeTo -> Edge
mkEdgeFrom nidFrom PartEdgeTo{to = nidTo, info = txt} = Edge{from = nidFrom, to = nidTo, info = txt}

mkEdgeTo :: NodeId -> PartEdgeFrom -> Edge
mkEdgeTo nidTo PartEdgeFrom{from = nidFrom, info = txt} = Edge{from = nidFrom, to = nidTo, info = txt}

instance FromJSON NodeId
instance ToJSON NodeId
