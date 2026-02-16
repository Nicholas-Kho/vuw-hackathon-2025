{-# LANGUAGE OverloadedRecordDot #-}

module TePapa.Client (
    ApiM (..),
    collectionsURL,
    getById,
    getRelated,
) where

import Api.TePapa
import Data.Proxy
import Servant.API
import Servant.Client
import TePapa.CommonObject (TePapaThing)
import TePapa.ExternalId (ExternalId (..), MuseumResource (..), TePapaReference (..))

class (Monad m) => ApiM m where
    runReq :: (ApiKey -> ClientM a) -> m (Either ClientError a)

collectionsURL :: BaseUrl
collectionsURL =
    BaseUrl
        { baseUrlScheme = Https
        , baseUrlPort = 443
        , baseUrlPath = "collection"
        , baseUrlHost = "data.tepapa.govt.nz"
        }

tePapaApi :: Proxy TePapaApi
tePapaApi = Proxy

getObject :: Int -> ApiKey -> ClientM TePapaThing
getAgent :: Int -> ApiKey -> ClientM TePapaThing
getPlace :: Int -> ApiKey -> ClientM TePapaThing
getCategory :: Int -> ApiKey -> ClientM TePapaThing
getTopic :: Int -> ApiKey -> ClientM TePapaThing
getObjectRelated :: Int -> Maybe Int -> ApiKey -> ClientM [TePapaThing]
getAgentRelated :: Int -> Maybe Int -> ApiKey -> ClientM [TePapaThing]
getPlaceRelated :: Int -> Maybe Int -> ApiKey -> ClientM [TePapaThing]
getConceptRelated :: Int -> Maybe Int -> ApiKey -> ClientM [TePapaThing]
getTopicRelated :: Int -> Maybe Int -> ApiKey -> ClientM [TePapaThing]
( getObject
        :<|> getAgent
        :<|> getPlace
        :<|> getCategory
        :<|> getTopic
        :<|> getObjectRelated
        :<|> getAgentRelated
        :<|> getPlaceRelated
        :<|> getConceptRelated
        :<|> getTopicRelated
    ) = client tePapaApi

getById :: TePapaReference -> ApiKey -> ClientM TePapaThing
getById tref key =
    let
        rawId = unId tref.eid
     in
        case tref.namespace of
            ObjectR -> getObject rawId key
            AgentR -> getAgent rawId key
            PlaceR -> getPlace rawId key
            ConceptR -> getCategory rawId key
            TopicR -> getTopic rawId key

getRelated :: TePapaReference -> Maybe Int -> ApiKey -> ClientM [TePapaThing]
getRelated tref limit key =
    let
        rawId = unId tref.eid
     in
        case tref.namespace of
            ObjectR -> getObjectRelated rawId limit key
            AgentR -> getAgentRelated rawId limit key
            PlaceR -> getPlaceRelated rawId limit key
            ConceptR -> getConceptRelated rawId limit key
            TopicR -> getTopicRelated rawId limit key
