{-# LANGUAGE DuplicateRecordFields #-}

module Domain.Model (
    Edge (..),
    GraphFragment (..),
    Node (..),
    NodeContent (..),
    NodeId,
    PartEdgeFrom (..),
    PartEdgeTo (..),
    mkEdgeFrom,
    mkEdgeTo,
    prettyPrintNodeId,
)
where

import qualified Data.Set as S
import Data.Text
import Servant.Client (ClientError)
import TePapa.Decode (TePapaReference, showTePapaReferenceNice)

type NodeId = TePapaReference

prettyPrintNodeId :: NodeId -> String
prettyPrintNodeId = showTePapaReferenceNice

data NodeContent = NodeContent
    { title :: Text
    , description :: Text
    , thumbnailUrl :: Maybe Text
    }
    deriving (Show)

data GraphFragment = GraphFragment
    { content :: NodeContent
    , outEdges :: S.Set PartEdgeTo
    , inEdges :: S.Set PartEdgeFrom
    }

data Node = Fail ClientError | Ok NodeContent deriving (Show)

data Edge = Edge
    { from :: NodeId
    , to :: NodeId
    , info :: Text
    }
    deriving (Eq, Ord, Show)

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
