{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api.TePapa where

import Data.Text
import GHC.Base
import Servant.API
import TePapa.Decode

newtype ApiKey = ApiKey {getKey :: Text} deriving (ToHttpApiData)

type ById endpoint responseType =
    endpoint
        :> Capture "id" Int
        :> Header' '[Required] "x-api-key" ApiKey
        :> Get '[JSON] responseType

type family ShowNamespace (ns :: MuseumResource) :: Symbol where
    ShowNamespace 'ObjectR = "object"
    ShowNamespace 'AgentR = "agent"
    ShowNamespace 'PlaceR = "place"
    ShowNamespace 'ConceptR = "category"
    ShowNamespace 'TopicR = "topic"

type RelatedApi (ns :: MuseumResource) =
    (ShowNamespace ns)
        :> Capture "id" Int
        :> "related"
        :> QueryParam "limit" Int
        :> Header' '[Required] "x-api-key" ApiKey
        :> Get '[JSON] RelatedThings

type TePapaApi =
    ById "object" ObjectResponse
        :<|> ById "agent" AgentResponse
        :<|> ById "place" Place
        :<|> RelatedApi 'ObjectR
        :<|> RelatedApi 'AgentR
        :<|> RelatedApi 'PlaceR
        :<|> RelatedApi 'ConceptR
        :<|> RelatedApi 'TopicR
