module Domain.Model (
    Edge (..),
    GraphFragment (..),
    Node (..),
    NodeContent (..),
    NodeId,
    PartEdge,
    partEdgeFrom,
    partEdgeTo,
)
where

import qualified Data.Set as S
import Data.Text
import Servant.Client (ClientError)
import TePapa.Decode (TePapaReference)

type NodeId = TePapaReference

data NodeContent = NodeContent
    { title :: Text
    , description :: Text
    , thumbnailUrl :: Maybe Text
    }

data GraphFragment = GraphFragment
    { content :: NodeContent
    , outEdges :: S.Set PartEdge
    , inEdges :: S.Set PartEdge
    }

data Node = Fail ClientError | Ok NodeContent

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
