{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-
This module contains a spec for a subset of the Te Papa Collections API.
The following API spec should not be confused for the our backend's API spec,
which lives in the API module.
See https://data.tepapa.govt.nz/docs/ and
https://github.com/te-papa/collections-api/wiki
-}
module ApiTepapa (ApiTepapa, agentById, objectById) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.Types (Value (..))
import Data.Text (Text, unpack)
import Models (Artefact (..), Collaboration (..), Organisation (..), Person (..), Specimen (..))
import Servant
import Servant.Client

type NeedsKey api = Header "x-api-key" Text :> api

type ApiTepapa =
    NeedsKey ("agent" :> Capture "id" Int :> Get '[JSON] AgentResponse)
        :<|> NeedsKey ("object" :> Capture "id" Int :> Get '[JSON] ObjectResponse)

data AgentResponse
    = APerson Person
    | AnOrg Organisation
    | ACollab Collaboration
    deriving (Show)

data ObjectResponse
    = AnArtefact Artefact
    | ASpecimen Specimen

instance FromJSON ObjectResponse where
    parseJSON = withObject "An /object response" $ \o -> do
        resType :: Text <- o .: "type"
        case resType of
            "Object" -> AnArtefact <$> parseJSON (Object o)
            "Specimen" -> ASpecimen <$> parseJSON (Object o)
            somethingElse ->
                fail $
                    "I don't know how to parse type="
                        <> unpack somethingElse
                        <> " ! I am expecting either Object or Specimen."

instance FromJSON AgentResponse where
    parseJSON = withObject "An /agent response" $ \o -> do
        resType :: Text <- o .: "type"
        case resType of
            "Person" -> APerson <$> parseJSON (Object o)
            "Organisation" -> AnOrg <$> parseJSON (Object o)
            -- I don't even think the API browser supports this one...
            "Collaboration" -> ACollab <$> parseJSON (Object o)
            somethingElse ->
                fail $
                    "I don't know how to parse type="
                        <> unpack somethingElse
                        <> " ! I am expecting either person, organisation, or collaboration."

agentById :: Maybe Text -> Int -> ClientM AgentResponse
objectById :: Maybe Text -> Int -> ClientM ObjectResponse
(agentById :<|> objectById) = client (Proxy @ApiTepapa)
