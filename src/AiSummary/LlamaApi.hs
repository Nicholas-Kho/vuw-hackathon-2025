module AiSummary.LlamaApi (LlamaM (..), checkHealth, describeThis, llamaUrl) where

import AiSummary.CompletionTypes (CompletionParams, CompletionResponse)
import AiSummary.FormatRequest (mkDescribeParams)
import AiSummary.Health
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import Servant.API
import Servant.Client
import TePapa.CommonObject (TePapaThing)

type LlamaApi =
    "health" :> Get '[JSON] HealthResponse
        :<|> "completion" :> ReqBody '[JSON] CompletionParams :> Post '[JSON] CompletionResponse

class (MonadIO m) => LlamaM m where
    llamaEnv :: m ClientEnv

checkHealth :: ClientM HealthResponse
prompt :: CompletionParams -> ClientM CompletionResponse
checkHealth :<|> prompt = client (Proxy @LlamaApi)

describeThis :: TePapaThing -> ClientM CompletionResponse
describeThis = prompt . mkDescribeParams

llamaUrl :: Int -> BaseUrl
llamaUrl localPort =
    BaseUrl
        { -- It's OK to use HTTP here because the server is running locally and is not exposed to the internet.
          baseUrlScheme = Http
        , baseUrlPort = localPort
        , baseUrlPath = ""
        , baseUrlHost = "localhost"
        }
