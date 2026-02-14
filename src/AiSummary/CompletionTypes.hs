{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module AiSummary.CompletionTypes (
    CompletionParams (..),
    CompletionResponse (..),
) where

import Data.Aeson
import Data.Text
import GHC.Generics

data CompletionParams = CompletionParams
    { prompt :: Text
    , maxTokens :: Int
    , repeatPenalty :: Float
    , temperature :: Float
    , topP :: Float
    }
    deriving (Generic)

instance ToJSON CompletionParams where
    toEncoding params =
        pairs
            ( "max_tokens" .= params.maxTokens
                <> "temperature" .= params.temperature
                <> "repeat_penalty" .= params.repeatPenalty
                <> "top_p" .= params.topP
                <> "prompt" .= params.prompt
            )

newtype CompletionResponse
    = CompletionResponse {toText :: Text}

instance FromJSON CompletionResponse where
    parseJSON =
        withObject
            "A llama /completion response"
            (\o -> o .: "content" >>= (pure . CompletionResponse))
