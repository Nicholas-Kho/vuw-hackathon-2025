module TePapa.Client (
    collectionsURL,
    getAgent,
    getObject,
    getPlace,
) where

import Api.TePapa
import Data.Proxy
import Data.Text
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

getObject :: Text -> Int -> ClientM ObjectResponse
getAgent :: Text -> Int -> ClientM AgentResponse
getPlace :: Text -> Int -> ClientM Place
(getObject :<|> getAgent :<|> getPlace) = client tePapaApi
