{-# LANGUAGE DeriveAnyClass #-}

module Api.Backend (
    BackendApi,
    InitialGameState (..),
    ExpandParams (..),
) where

import Cache.NodeId (NodeId)
import Data.Aeson
import Domain.Model
import GHC.Generics
import Servant.API

type BackendApi =
    "start" :> Get '[JSON] InitialGameState
        :<|> "expand" :> ReqBody '[JSON] ExpandParams :> Post '[JSON] [(NodeId, Node)]

data InitialGameState = InitialGameState
    { startAt :: NodeId
    , endAt :: NodeId
    , subgraph :: [(NodeId, Node)]
    }
    deriving (Generic, ToJSON)

data ExpandParams = ExpandParams
    { expandAboutId :: NodeId
    , iAlreadyHave :: [NodeId]
    }
    deriving (Generic, FromJSON)
