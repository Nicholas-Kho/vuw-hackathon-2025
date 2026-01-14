{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.TePapa where

import Data.Text
import Servant.API
import TePapa.Decode

newtype ApiKey = ApiKey {getKey :: Text} deriving (ToHttpApiData)

type ById endpoint responseType =
    endpoint
        :> Capture "id" Int
        :> Header' '[Required] "x-api-key" ApiKey
        :> Get '[JSON] responseType

type TePapaApi =
    ById "object" ObjectResponse
        :<|> ById "agent" AgentResponse
        :<|> ById "place" Place
