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

newtype EdgeInfo = EdgeInfo {text :: T.Text}
    deriving (Eq, Ord, Show)

data Node = Node
    { content :: NodeContent
    , incomingEdges :: M.Map NodeId (S.Set EdgeInfo)
    , outgoingEdges :: M.Map NodeId (S.Set EdgeInfo)
    }
    deriving (Generic)

-- A more Elm-friendly representation of node which it can parse from JSON.
data NodeElm = NodeElm
    { content :: NodeContent
    , incomingEdges :: [(NodeId, [EdgeInfo])]
    , outgoingEdges :: [(NodeId, [EdgeInfo])]
    }
    deriving (Generic)

elmify :: Node -> NodeElm
elmify
    Node
        { content = c
        , incomingEdges = inc
        , outgoingEdges = out
        } =
        NodeElm
            { content = c
            , incomingEdges = changeMap inc
            , outgoingEdges = changeMap out
            }
      where
        changeMap m = (\(nid, s) -> (nid, S.elems s)) <$> M.toList m

mkNode :: NodeContent -> Node
mkNode nc =
    Node
        { content = nc
        , incomingEdges = M.empty
        , outgoingEdges = M.empty
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
