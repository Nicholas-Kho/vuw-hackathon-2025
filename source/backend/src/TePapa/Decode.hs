{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Decode (
    RawItem,
    Person,
    Artefact,
    Specimen,
    Organization,
    ObjectResponse,
)
where

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

data ObjectResponse
    = Art Artefact
    | Spc Specimen

instance FromJSON ObjectResponse where
    parseJSON =
        withObject
            "response from /object endpoint"
            ( \o -> do
                common :: CommonFields <- parseJSON (Data.Aeson.Types.Object o)
                case classLabel common of
                    "Object" -> Art <$> parseRemainingArtefact common o
                    "Specimen" -> Spc <$> parseRemainingSpecimen common o
                    other ->
                        fail $ "I am expecting a 'type' of 'Object' or 'Specimen' from the /object endpoint, but I got " <> (Prelude.show other) <> "!"
            )

parseRemainingSpecimen :: CommonFields -> Object -> Parser Specimen
parseRemainingSpecimen cf o =
    Specimen
        <$> (pure cf)
        <*> o .: "collectionLabel"
        <*> o .:? "captionFormatted" .!= ""

parseRemainingArtefact :: CommonFields -> Object -> Parser Artefact
parseRemainingArtefact cf o =
    Artefact
        <$> (pure cf)
        <*> o .: "collectionLabel"
        <*> o .: "caption"
