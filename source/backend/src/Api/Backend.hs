{-# LANGUAGE DeriveAnyClass #-}

module Api.Backend (
    BackendApi,
    ClientGraphAction (..),
    InitialGameState (..),
    ExpandParams (..),
) where

import Data.Aeson
import Domain.Model
import GHC.Generics
import Servant.API

type BackendApi =
    "start" :> Get '[JSON] InitialGameState
        :<|> "expand" :> ReqBody '[JSON] ExpandParams :> Post '[JSON] [ClientGraphAction]

data ClientGraphAction
    = AddNode NodeId NodeContent
    | AddEdge Edge
    deriving (Show, Generic, ToJSON)

data InitialGameState = InitialGameState
    { startAt :: NodeId
    , endAt :: NodeId
    , subgraph :: [ClientGraphAction]
    }
    deriving (Generic, ToJSON)

data ExpandParams = ExpandParams
    { expandAboutId :: NodeId
    , iAlreadyHave :: [NodeId]
    }
    deriving (Generic, FromJSON)
