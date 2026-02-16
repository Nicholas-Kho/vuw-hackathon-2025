{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api.TePapa where

import Data.Text
import GHC.Base
import Servant.API
import TePapa.CommonObject (TePapaThing)
import TePapa.ExternalId

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
        :> Get '[JSON] [TePapaThing]

type TePapaApi =
    ById "object" TePapaThing
        :<|> ById "agent" TePapaThing
        :<|> ById "place" TePapaThing
        :<|> ById "category" TePapaThing
        :<|> ById "topic" TePapaThing
        :<|> RelatedApi 'ObjectR
        :<|> RelatedApi 'AgentR
        :<|> RelatedApi 'PlaceR
        :<|> RelatedApi 'ConceptR
        :<|> RelatedApi 'TopicR
