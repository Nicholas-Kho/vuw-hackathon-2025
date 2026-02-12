{-# LANGUAGE OverloadedStrings #-}

module AiSummary.Health (
    HealthResponse (..),
) where

import Data.Aeson
import Data.Text

{-
From the Llama server docs:
Response format
    HTTP status code 503
        Body: {"error": {"code": 503, "message": "Loading model", "type": "unavailable_error"}}
        Explanation: the model is still being loaded.
    HTTP status code 200
        Body: {"status": "ok" }
        Explanation: the model is successfully loaded and the server is ready.
Servant will throw a clienterr in the former case, so we only make a JSON parser for the latter.
Servant-checked-exceptions seems overkill for now.
-}

data HealthResponse
    = Loading
    | Ok

instance FromJSON HealthResponse where
    parseJSON =
        withObject
            "A Health response"
            ( \o -> do
                status :: Text <- o .: "status"
                case status of
                    "ok" -> pure Ok
                    _ -> fail "Status was not OK"
            )
