{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{-
This module contains a spec for a subset of the Te Papa Collections Api, not
the one that our backend exposes.
See https://data.tepapa.govt.nz/docs/ and
https://github.com/te-papa/collections-api/wiki
-}
module ApiTepapa () where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson.TH
import Data.Aeson.Types (Parser, Value (..))
import Data.Text (Text)
import Servant

type ApiTepapa =
    "agent" :> Capture "id" Int :> Get '[JSON] AgentResponse

-- A reference given to us by the API. Yet to be stored in the DB.
data ReferenceRP = ReferenceRP
    { getExternalId :: Int
    , getResourceType :: Text
    }

-- A person response given to us by the API. Yet to be stored in the DB.
-- when it is, the references should be resolved to DB reference IDs.
data PersonRP = PersonRP
    { title :: Text
    , verbatimBirthDate :: String
    , verbatimDeathDate :: String
    , bithPlace :: String
    , deathPlace :: String
    , -- Identifier of a Specimen
      identificationIdentified :: [ReferenceRP]
    , -- Maker of an Object
      productionContributor :: [ReferenceRP]
    , -- Associated with another agent
      associatedParties :: [ReferenceRP]
    , -- Author of a Topic or Publication
      authors :: [ReferenceRP]
    , -- Recorder of the field collection of a Specimen
      evidenceForAtEventRecordedBy :: [ReferenceRP]
    , -- Author of the scientific name of a Specimen
      scientificNameAuthorship :: [ReferenceRP]
    , -- Associated with another agent, Category, or Place
      associatedWith :: [ReferenceRP]
    , -- Depicted in an Object
      depicts :: [ReferenceRP]
    , -- Former owner of an Object
      formerOwner :: [ReferenceRP]
    , -- Referred to by an Object, Specimen, Topic, or Publication
      refersTo :: [ReferenceRP]
    , -- Influenced the making of an Object
      influencedBy :: [ReferenceRP]
    , -- Publisher of a Publication
      publisher :: [ReferenceRP]
    , -- Editor of a Publication
      editor :: [ReferenceRP]
    , -- Illustrator of a Publication
      illustrator :: [ReferenceRP]
    , -- Part of an aggregation of agents
      aggregatedAgents :: [ReferenceRP]
    , -- Associated in an unspecified way with an Object or Specimen
      unknownAssociation :: [ReferenceRP]
    }

-- TODO: This can probably be shrunk with a helper function.
instance FromJSON PersonRP where
    parseJSON = withObject "Te Papa person object" $ \o ->
        PersonRP
            <$> o .: "title"
            <*> o .:? "verbatimBirthDate" .!= "birthday unknown"
            <*> o .:? "verbatimDeathDate" .!= "death date unknown"
            <*> o .:? "birthPlace" .!= "birthplace unknown"
            <*> o .:? "deathPlace" .!= "death place unknown"
            <*> o .:? "identificationIdentified" .!= [] -- Identifier of a Specimen
            <*> o .:? "productionContributor" .!= [] -- Maker of an Object
            <*> o .:? "associatedParties" .!= [] -- Associated with another agent
            <*> o .:? "authors" .!= [] -- Author of a Topic or Publication
            <*> o .:? "evidenceForAtEventRecordedBy" .!= [] -- Recorder of the field collection of a Specimen
            <*> o .:? "scientificNameAuthorship" .!= [] -- Author of the scientific name of a Specimen
            <*> o .:? "associatedWith" .!= [] -- Associated with another agent, Category, or Place
            <*> o .:? "depicts" .!= [] -- Depicted in an Object
            <*> o .:? "formerOwner" .!= [] -- Former owner of an Object
            <*> o .:? "refersTo" .!= [] -- Referred to by an Object, Specimen, Topic, or Publication
            <*> o .:? "influencedBy" .!= [] -- Influenced the making of an Object
            <*> o .:? "publisher" .!= [] -- Publisher of a Publication
            <*> o .:? "editor" .!= [] -- Editor of a Publication
            <*> o .:? "illustrator" .!= [] -- Illustrator of a Publication
            <*> o .:? "aggregatedAgents" .!= [] -- Part of an aggregation of agents
            <*> o .:? "unknownAssociation" .!= [] -- Associated in an unspecified way with an Object or Specimen

instance FromJSON ReferenceRP where
    parseJSON = withObject "Te Papa object reference" $ \o ->
        ReferenceRP
            <$> o .: "id"
            <*> o .: "type"

data AgentResponse
    = APerson PersonRP

-- \| AnOrg OrganizationRP
-- \| ACollab CollaborationRP

instance FromJSON AgentResponse where
    parseJSON = withObject "agent response" $ \o -> do
        resourceType :: String <- o .: "type"
        case resourceType of
            -- Call into Person parser here
            "Person" -> APerson <$> parseJSON (Object o)
            -- Call into Organization parser here
            -- "Organization" -> AnOrg <$> undefined
            -- Call into Collaboration parser here
            -- "Collaboration" -> ACollab <$> undefined
            somethingElse -> fail $ "Agent endpoint returned " <> somethingElse <> ", which I don't know how to handle!"
