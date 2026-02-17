{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Model (
    EdgeInfo (..),
    Node (..),
    NodeContent (..),
    NodeElm (..),
    elmify,
    mkNode,
    prettyPrintNode,
)
where

import Cache.NodeId (NodeId)
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as M
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

newtype EdgeInfo = EdgeInfo {text :: T.Text}
    deriving (Eq, Ord, Show)

instance Semigroup EdgeInfo where
    (EdgeInfo t1) <> (EdgeInfo t2) = EdgeInfo (t1 <> t2)

data Node = Node
    { content :: NodeContent
    , outgoingEdges :: M.Map NodeId EdgeInfo
    , nodeId :: NodeId
    }
    deriving (Generic)

-- A more Elm-friendly representation of node which it can parse from JSON.
data NodeElm = NodeElm
    { content :: NodeContent
    , outgoingEdges :: [(NodeId, EdgeInfo)]
    , nodeId :: NodeId
    }
    deriving (Generic)

elmify :: Node -> NodeElm
elmify
    Node
        { content = c
        , outgoingEdges = out
        , nodeId = nid
        } =
        NodeElm
            { content = c
            , outgoingEdges = M.toList out
            , nodeId = nid
            }

mkNode :: NodeId -> NodeContent -> Node
mkNode nid nc =
    Node
        { content = nc
        , outgoingEdges = M.empty
        , nodeId = nid
        }

prettyPrintNode :: NodeContent -> String
prettyPrintNode nc =
    (unpack . title $ nc)
        <> ": "
        <> (Prelude.take 15 . unpack . description $ nc)
        <> (if (Data.Text.length . description $ nc) > 15 then "..." else "")

deriveBoth defaultOptions ''EdgeInfo
deriveBoth defaultOptions ''NodeContent
deriveBoth defaultOptions ''NodeElm
