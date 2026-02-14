{-# LANGUAGE OverloadedRecordDot #-}

module AiSummary.Preprocess where

import AiSummary.CompletionTypes (CompletionParams (..))
import Data.Text

data Settings = Settings
    { temperature :: Float
    , maxTokens :: Int
    , repeatPenalty :: Float
    , topP :: Float
    }

defaultSettings :: Settings
defaultSettings =
    Settings
        { temperature = 0.1
        , maxTokens = 120
        , repeatPenalty = 1.1
        , topP = 0.9
        }

requestFromRawPrompt :: Text -> CompletionParams
requestFromRawPrompt prompt =
    let
        settings = defaultSettings
     in
        CompletionParams
            { topP = settings.topP
            , temperature = settings.temperature
            , repeatPenalty = settings.repeatPenalty
            , prompt = prompt
            , maxTokens = settings.maxTokens
            }
