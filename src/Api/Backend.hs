{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Backend (
    ApiRoutes,
    BackendApi,
    InitialGameState (..),
    ExpandParams (..),
    Subgraph (..),
    UnverifiedNodeId (..),
) where

import Cache.NodeId (NodeId)
import Domain.Model
import GHC.Generics
import Servant.API
import Servant.Elm

-- The api/expand endpoint returns a Maybe because the API caller is not trusted, and so
-- we need to verify their input first. However, we make the guarantee that all node IDs
-- we send are valid. Therefore, clients may assume that this function will not return null if it is
-- passed an id that was given to them at any point from this API.

type ApiRoutes =
    "start" :> Get '[JSON] InitialGameState
        :<|> "expand" :> ReqBody '[JSON] ExpandParams :> Post '[JSON] (Maybe Subgraph)

type BackendApi =
    "api" :> ApiRoutes
        :<|> Raw

data Subgraph = Subgraph
    { contents :: [(NodeId, NodeElm)]
    }
    deriving (Generic)

data InitialGameState = InitialGameState
    { startAt :: NodeId
    , endAt :: NodeId
    , subgraph :: Subgraph
    }
    deriving (Generic)

data ExpandParams = ExpandParams
    { expandAboutId :: UnverifiedNodeId
    }
    deriving (Generic)

newtype UnverifiedNodeId = UnverifiedNodeId {toInt :: Int}
    deriving (Generic)

deriveBoth defaultOptions ''Subgraph
deriveBoth defaultOptions ''InitialGameState
deriveBoth defaultOptions ''UnverifiedNodeId
deriveBoth defaultOptions ''ExpandParams
