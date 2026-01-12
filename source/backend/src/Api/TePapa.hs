{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.TePapa where

import Data.Text
import Servant.API
import TePapa.Decode

type NeedsKey api = Header' '[Required] "x-api-key" Text :> api

type ById endpoint responseType =
    NeedsKey (endpoint :> Capture "id" Int :> Get '[JSON] responseType)

type TePapaApi =
    ById "object" ObjectResponse
        :<|> ById "agent" AgentResponse
        :<|> ById "place" Place
