module AiSummary.LlamaApi (checkHealth, llamaUrl, prompt) where

import AiSummary.Health
import Data.Proxy
import Servant.API
import Servant.Client

type CompletionParams = ()
type CompletionResponse = ()

type LlamaApi =
    "health" :> Get '[JSON] HealthResponse
        :<|> "completion" :> ReqBody '[JSON] CompletionParams :> Post '[JSON] CompletionResponse

checkHealth :: ClientM HealthResponse
prompt :: CompletionParams -> ClientM CompletionResponse
checkHealth :<|> prompt = client (Proxy @LlamaApi)

llamaUrl :: Int -> BaseUrl
llamaUrl localPort =
    BaseUrl
        { -- It's OK to use HTTP here because the server is running locally and is not exposed to the internet.
          baseUrlScheme = Http
        , baseUrlPort = localPort
        , baseUrlPath = ""
        , baseUrlHost = "localhost"
        }
