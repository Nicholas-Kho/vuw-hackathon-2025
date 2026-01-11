module TePapa.Client () where

import Api.TePapa
import Data.Proxy
import Data.Text
import Servant.Client
import TePapa.Decode

tePapaApi :: Proxy TePapaApi
tePapaApi = Proxy

getObject :: Text -> Int -> ClientM ObjectResponse
getObject = client tePapaApi
