module TePapa.Client (
    ApiM (..),
    collectionsURL,
    getAgent,
    getAgentRelated,
    getCategory,
    getConceptRelated,
    getObject,
    getObjectRelated,
    getPlace,
    getPlaceRelated,
    getTopicRelated,
) where

import Api.TePapa
import Data.Proxy
import Servant.API
import Servant.Client
import TePapa.Decode

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

getObject :: Int -> ApiKey -> ClientM ObjectResponse
getAgent :: Int -> ApiKey -> ClientM AgentResponse
getPlace :: Int -> ApiKey -> ClientM Place
getCategory :: Int -> ApiKey -> ClientM Category
getObjectRelated :: Int -> Maybe Int -> ApiKey -> ClientM RelatedThings
getAgentRelated :: Int -> Maybe Int -> ApiKey -> ClientM RelatedThings
getPlaceRelated :: Int -> Maybe Int -> ApiKey -> ClientM RelatedThings
getConceptRelated :: Int -> Maybe Int -> ApiKey -> ClientM RelatedThings
getTopicRelated :: Int -> Maybe Int -> ApiKey -> ClientM RelatedThings
( getObject
        :<|> getAgent
        :<|> getPlace
        :<|> getCategory
        :<|> getObjectRelated
        :<|> getAgentRelated
        :<|> getPlaceRelated
        :<|> getConceptRelated
        :<|> getTopicRelated
    ) = client tePapaApi

class (Monad m) => ApiM m where
    runReq :: (ApiKey -> ClientM a) -> m (Either ClientError a)
