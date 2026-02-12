module AiSummary.LlamaApi where

import AiSummary.Health
import Servant.API

type CompletionParams = ()
type CompletionResponse = ()

type LlamaApi =
    "health" :> Get '[JSON] HealthResponse
        :<|> "completion" :> ReqBody '[JSON] CompletionParams :> Post '[JSON] CompletionResponse
