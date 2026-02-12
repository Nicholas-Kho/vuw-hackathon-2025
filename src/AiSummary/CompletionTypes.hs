module AiSummary.CompletionTypes (
    CompletionParams (..),
    CompletionResponse (..),
) where

import Data.Text

data CompletionParams = CompletionParams
    { model :: Text
    , systemPrompt :: Text
    , userPrompt :: Text
    , maxTokens :: Int
    , temperature :: Float
    }
type CompletionResponse = ()
