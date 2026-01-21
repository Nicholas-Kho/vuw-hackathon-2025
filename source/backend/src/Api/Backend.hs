{-# LANGUAGE DeriveAnyClass #-}

module Api.Backend (
    BackendApi,
    InitialGameState (..),
    ExpandParams (..),
) where

import Cache.Interface (GraphAction)
import Data.Aeson
import Domain.Model
import GHC.Generics
import Servant.API

type BackendApi =
    "start" :> Get '[JSON] InitialGameState
        :<|> "expand" :> ReqBody '[JSON] ExpandParams :> Post '[JSON] [GraphAction]

data InitialGameState = InitialGameState
    { startAt :: NodeId
    , endAt :: NodeId
    , subgraph :: [GraphAction]
    }
    deriving (Generic, ToJSON)

data ExpandParams = ExpandParams
    { expandAboutId :: NodeId
    , iAlreadyHave :: [NodeId]
    }
    deriving (Generic, FromJSON)
