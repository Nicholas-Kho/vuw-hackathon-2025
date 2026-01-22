{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Model (
    EdgeInfo,
    Node (..),
    NodeContent (..),
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
    , incomingEdges :: M.Map NodeId (S.Set EdgeInfo)
    }
    deriving (Show, Eq, Generic, ToJSON, Hashable)

data Node
    = Unexpanded NodeContent
    | Expanded NodeContent (M.Map NodeId (S.Set EdgeInfo))
    deriving (Eq, Generic, Hashable, ToJSON)

type EdgeInfo = T.Text

prettyPrintNode :: NodeContent -> String
prettyPrintNode nc =
    (unpack . title $ nc)
        <> ": "
        <> (Prelude.take 15 . unpack . description $ nc)
        <> (if (Data.Text.length . description $ nc) > 15 then "..." else "")
