{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.TePapa where

import Data.Text
import Servant.API
import TePapa.Decode

type NeedsKey api = Header' '[Required] "x-api-key" Text :> api

type TePapaApi =
    NeedsKey ("object" :> Capture "id" Int :> Get '[JSON] ObjectResponse)
        :<|> NeedsKey ("agent" :> Capture "id" Int :> Get '[JSON] AgentResponse)
