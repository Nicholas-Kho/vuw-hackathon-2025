{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{-
This module contains a spec for a subset of the Te Papa Collections API.
The following API spec should not be confused for the our backend's API spec,
which lives in the API module.
See https://data.tepapa.govt.nz/docs/ and
https://github.com/te-papa/collections-api/wiki
-}
module ApiTepapa (ApiTepapa) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.Types (Value (..))
import Data.Text (Text, unpack)
import Models (Collaboration (..), Organization (..), Person (..))
import Servant

type ApiTepapa =
    "agent" :> Capture "id" Int :> Get '[JSON] AgentResponse

data AgentResponse
    = APerson Person
    | AnOrg Organization
    | ACollab Collaboration

instance FromJSON AgentResponse where
    parseJSON = withObject "An /agent response" $ \o -> do
        resType :: Text <- o .: "type"
        case resType of
            "person" -> APerson <$> parseJSON (Object o)
            "organisation" -> AnOrg <$> fail "Organisations not implemented"
            "collaboration" -> ACollab <$> fail "Collaborations not implemented"
            somethingElse ->
                fail $
                    "I don't know how to parse type="
                        <> unpack somethingElse
                        <> " ! I am expecting either person, organisation, or collaboration."
