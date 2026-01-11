module TePapa.Client (
    collectionsURL,
    getObject,
) where

import Api.TePapa
import Data.Proxy
import Data.Text
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
getObject = client tePapaApi
