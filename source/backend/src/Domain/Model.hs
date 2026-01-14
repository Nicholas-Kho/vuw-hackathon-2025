module Domain.Model (
    Edge (..),
    Node (..),
    NodeContent (..),
    NodeId,
    PartEdge,
    partEdgeFrom,
    partEdgeTo,
)
where

import Data.Text
import Servant.Client (ClientError)
import TePapa.Decode (TePapaReference)

type NodeId = TePapaReference

data NodeContent = NodeContent
    { title :: Text
    , description :: Text
    , thumbnailUrl :: Maybe Text
    }

data Node
    = NotFetched
    | Fetching
    | Ok NodeContent
    | -- Note: Might want to change to something else later so
      -- we're not coupled to Servant
      Fail ClientError

data Edge = Edge
    { from :: NodeId
    , to :: NodeId
    , info :: Text
    }
    deriving (Eq, Ord, Show)

type PartEdge = (NodeId, Text)

partEdgeFrom :: NodeId -> PartEdge -> Edge
partEdgeFrom nidFrom (nidTo, txt) = Edge{from = nidFrom, to = nidTo, info = txt}

partEdgeTo :: NodeId -> PartEdge -> Edge
partEdgeTo nidTo (nidFrom, txt) = Edge{from = nidFrom, to = nidTo, info = txt}
