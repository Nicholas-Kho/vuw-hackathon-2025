{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Model (
    EdgeInfo,
    Node (..),
    NodeContent (..),
    addIncoming,
    addOutgoing,
    mkNode,
    prettyPrintNode,
)
where

import Cache.NodeId (NodeId)
import Data.Aeson
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text
import qualified Data.Text as T
import GHC.Generics

data NodeContent = NodeContent
    { title :: Text
    , description :: Text
    , thumbnailUrl :: Maybe Text
    }
    deriving (Show, Eq, Generic, ToJSON, Hashable)

data Node = Node
    { content :: NodeContent
    , incomingEdges :: M.Map NodeId (S.Set EdgeInfo)
    , outgoingEdges :: M.Map NodeId (S.Set EdgeInfo)
    }
    deriving (Generic, ToJSON)

mkNode :: NodeContent -> Node
mkNode nc =
    Node
        { content = nc
        , incomingEdges = M.empty
        , outgoingEdges = M.empty
        }

addIncoming :: Node -> NodeId -> EdgeInfo -> Node
addIncoming n nfrom e =
    n
        { incomingEdges = M.insertWith S.union nfrom (S.singleton e) (incomingEdges n)
        }

addOutgoing :: Node -> NodeId -> EdgeInfo -> Node
addOutgoing n nto e =
    n
        { incomingEdges = M.insertWith S.union nto (S.singleton e) (outgoingEdges n)
        }

type EdgeInfo = T.Text

prettyPrintNode :: NodeContent -> String
prettyPrintNode nc =
    (unpack . title $ nc)
        <> ": "
        <> (Prelude.take 15 . unpack . description $ nc)
        <> (if (Data.Text.length . description $ nc) > 15 then "..." else "")
