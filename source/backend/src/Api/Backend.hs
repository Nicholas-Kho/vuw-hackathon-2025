{-# LANGUAGE DeriveAnyClass #-}

module Api.Backend (
    BackendApi,
    InitialGameState (..),
    ExpandParams (..),
    UnverifiedNodeId (..),
) where

import Cache.NodeId (NodeId)
import Data.Aeson
import Domain.Model
import GHC.Generics
import Servant.API

type BackendApi =
    "start" :> Get '[JSON] InitialGameState
        -- This endpoint returns a Maybe because the API caller is not trusted, and so
        -- we need to verify their input first. However, we make the guarantee that all node IDs
        -- we send are valid. Therefore, clients may assume that this function will not return null if it is
        -- passed an id that was given to them at any point from this API.
        :<|> "expand" :> ReqBody '[JSON] ExpandParams :> Post '[JSON] (Maybe [(NodeId, Node)])

data InitialGameState = InitialGameState
    { startAt :: NodeId
    , endAt :: NodeId
    , subgraph :: [(NodeId, Node)]
    }
    deriving (Generic, ToJSON)

data ExpandParams = ExpandParams
    { expandAboutId :: UnverifiedNodeId
    }
    deriving (Generic, FromJSON)

newtype UnverifiedNodeId = UnverifiedNodeId {toInt :: Int}
    deriving (Generic)

instance FromJSON UnverifiedNodeId
