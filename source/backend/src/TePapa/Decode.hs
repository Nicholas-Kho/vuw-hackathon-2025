{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Decode (RawItem) where

import Data.Aeson
import Data.Aeson.KeyMap
import Data.Aeson.Types
import Data.Text
import Data.Traversable
import Data.Vector
import GHC.Generics

-- TODO: remove me!
data RawItem = RawItem {title :: Text} deriving (Generic)

instance FromJSON RawItem where
    parseJSON = genericParseJSON defaultOptions

-- NOTE: This is for ID namespaces, not other semantics.
-- The type is deliberately shallow.
data MuseumResource
    = Object
    | Agent
    | Place
    | Concept
    | Topic
    deriving (Show)

newtype ExternalId
    = ExternalId {unId :: Int}
    deriving (Show, FromJSON)

data TePapaReference = TePapaReference
    { namespace :: MuseumResource
    , eid :: ExternalId
    , title :: Text
    }
    deriving (Show, Generic)

data Association = Association
    { name :: Text
    , pointsTo :: [TePapaReference]
    }
    deriving (Show, Generic)

data EdgeDirection = Incoming | Outgoing deriving (Show, Generic)
data Edge = Edge {direction :: EdgeDirection, association :: Association} deriving (Show, Generic)

data CommonFields = CommonFields
    { eid :: ExternalId
    , title :: Text
    , classLabel :: Text
    , outgoing :: [Edge]
    }
    deriving (Show, Generic)

instance FromJSON CommonFields where
    parseJSON =
        withObject
            "Common fields to TePapa resource"
            ( \o ->
                CommonFields
                    <$> o .: "id"
                    <*> o .: "title"
                    <*> o .: "type"
                    <*> (pure $ parseCommonOutgoingEdges o)
            )

parseCommonOutgoingEdges :: Object -> [Edge]
parseCommonOutgoingEdges = elems . mapMaybeWithKey (\k v -> parseMaybe (edgeParser k) v)
  where
    edgeParser k v = Edge Outgoing <$> associationParserHelper k v

associationParserHelper :: Key -> Value -> Parser Association
associationParserHelper k =
    withArray
        "association"
        ( \a ->
            Association
                <$> (pure . Data.Text.show $ k)
                <*> (Data.Traversable.traverse parseReferenceyObject (Data.Vector.toList a))
        )

parseReferenceyObject :: Value -> Parser TePapaReference
parseReferenceyObject =
    withObject
        "Referencey object"
        ( \o ->
            TePapaReference
                <$> (o .: "type" >>= classLabelToResource)
                <*> o .: "id"
                <*> o .:? "title" .!= ""
        )

classLabelToResource :: Text -> Parser MuseumResource
classLabelToResource = \case
    "Object" -> pure TePapa.Decode.Object
    "Specimen" -> pure TePapa.Decode.Object
    "Person" -> pure Agent
    "Organisation" -> pure Agent
    "Place" -> pure Place
    "Category" -> pure Concept
    "Topic" -> pure Topic
    other -> fail $ "I can't map " <> (Prelude.show other) <> " to a museum resource type."

-- Agents
data Person = Person
    { com :: CommonFields
    , gender :: Text
    , familyName :: Text
    , givenName :: Text
    , verbatimBirthDate :: Text
    , verbatimDeathDate :: Text
    }

data Organization = Organization
    { com :: CommonFields
    , verbatimBirthDae :: Text
    , verbatimDeathDate :: Text
    }

-- Objects
data Specimen = Specimen
    { com :: CommonFields
    , collectionLabel :: Text
    , captionFormatted :: Text
    }

-- Called "object" in the documentation, I am calling it an "artefact" to avoid confusion with the resource name.
data Artefact = Artefact
    { com :: CommonFields
    , collectionLabel :: Text
    , caption :: Text
    }
