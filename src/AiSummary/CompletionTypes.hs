{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module AiSummary.CompletionTypes (
    CompletionParams (..),
    CompletionResponse (..),
) where

import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Text
import GHC.Generics

data CompletionParams = CompletionParams
    { model :: Text
    , systemPrompt :: Text
    , userPrompt :: Text
    , maxTokens :: Int
    , temperature :: Float
    }
    deriving (Generic)

data Role = System | User deriving (Generic)

instance ToJSON Role where
    toEncoding r =
        case r of
            System -> text "system"
            User -> text "user"

data Message = Message
    { role :: Role
    , content :: Text
    }
    deriving (Generic, ToJSON)

instance ToJSON CompletionParams where
    toEncoding params =
        pairs
            ( "model" .= params.model
                <> "max_tokens" .= params.maxTokens
                <> "temperature" .= params.temperature
                <> "messages"
                    .= [ Message System params.systemPrompt
                       , Message User params.userPrompt
                       ]
            )

newtype CompletionResponse
    = CompletionResponse {toText :: Text}

instance FromJSON CompletionResponse where
    parseJSON =
        withObject
            "A llama /completion response"
            (\o -> o .: "content" >>= (pure . CompletionResponse))
