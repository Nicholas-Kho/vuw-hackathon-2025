{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Model (
    EdgeInfo (..),
    Node (..),
    NodeContent (..),
    addIncoming,
    addOutgoing,
    mkNode,
    prettyPrintNode,
)
where

import Cache.NodeId (NodeId)
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text
import qualified Data.Text as T
import GHC.Generics
import Servant.Elm

data NodeContent = NodeContent
    { title :: Text
    , description :: Text
    , thumbnailUrl :: Maybe Text
    }
    deriving (Show, Eq, Generic, Hashable)

data Node = Node
    { content :: NodeContent
    , incomingEdges :: M.Map NodeId (S.Set EdgeInfo)
    , outgoingEdges :: M.Map NodeId (S.Set EdgeInfo)
    }
    deriving (Generic)

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

newtype EdgeInfo = EdgeInfo {text :: T.Text}
    deriving (Eq, Ord, Show)

prettyPrintNode :: NodeContent -> String
prettyPrintNode nc =
    (unpack . title $ nc)
        <> ": "
        <> (Prelude.take 15 . unpack . description $ nc)
        <> (if (Data.Text.length . description $ nc) > 15 then "..." else "")

deriveBoth defaultOptions ''EdgeInfo
deriveBoth defaultOptions ''NodeContent
deriveBoth defaultOptions ''Node
