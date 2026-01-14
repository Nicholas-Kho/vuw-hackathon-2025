module TePapa.Client (
    ApiM (..),
    collectionsURL,
    getAgent,
    getObject,
    getPlace,
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
(getObject :<|> getAgent :<|> getPlace) = client tePapaApi

class ApiM m where
    runReq :: (ApiKey -> ClientM a) -> m a
