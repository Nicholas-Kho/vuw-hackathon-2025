{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Decode (
    AgentResponse (..),
    Association (..),
    Person (..),
    Artefact (..),
    CommonFields (..),
    RelatedThings (..),
    Edge (..),
    EdgeDirection (..),
    Specimen (..),
    Organization (..),
    ObjectResponse (..),
    Place (..),
    TePapaReference (..),
    MuseumResource (..),
    ExternalId (..),
    Geolocation (..),
    showTePapaReferenceNice,
)
where

import Data.Aeson
import Data.Aeson.KeyMap
import Data.Aeson.Types
import Data.Hashable (Hashable)
import Data.Text
import Data.Traversable
import Data.Vector
import GHC.Generics

-- NOTE: This is for ID namespaces, not other semantics.
-- The type is deliberately shallow.
data MuseumResource
    = ObjectR
    | AgentR
    | PlaceR
    | ConceptR
    | TopicR
    deriving (Show, Eq, Ord, Generic)

instance Hashable MuseumResource

newtype ExternalId
    = ExternalId {unId :: Int}
    deriving (Show, FromJSON, Generic, Eq, Ord)

instance Hashable ExternalId

data TePapaReference = TePapaReference
    { namespace :: MuseumResource
    , eid :: ExternalId
    }
    deriving (Show, Generic, Eq, Ord)

instance Hashable TePapaReference

showTePapaReferenceNice :: TePapaReference -> String
showTePapaReferenceNice tref =
    let
        namespaceNice :: String = case (namespace tref) of
            ObjectR -> "object/"
            AgentR -> "agent/"
            PlaceR -> "place/"
            ConceptR -> "concept/"
            TopicR -> "topic/"
        idStr :: String = Prelude.show . unId $ tref.eid
     in
        namespaceNice <> idStr

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
        )

classLabelToResource :: Text -> Parser MuseumResource
classLabelToResource = \case
    "Object" -> pure ObjectR
    "Specimen" -> pure ObjectR
    "Person" -> pure AgentR
    "Organisation" -> pure AgentR
    "Place" -> pure PlaceR
    "Category" -> pure ConceptR
    "Topic" -> pure TopicR
    other -> fail $ "I can't map " <> (Prelude.show other) <> " to a museum resource type."

-- Agents
data Person = Person
    { com :: CommonFields
    , gender :: Text
    , familyName :: Text
    , givenName :: Text
    , verbatimBirthDate :: Text
    , verbatimDeathDate :: Maybe Text
    }
    deriving (Show)

data Organization = Organization
    { com :: CommonFields
    , verbatimBirthDate :: Text
    , verbatimDeathDate :: Maybe Text
    }
    deriving (Show)

-- Objects
data Specimen = Specimen
    { com :: CommonFields
    , collectionLabel :: Text
    , captionFormatted :: Text
    }
    deriving (Show)

-- Called "object" in the documentation, I am calling it an "artefact" to avoid confusion with the resource name.
data Artefact = Artefact
    { com :: CommonFields
    , collectionLabel :: Text
    , caption :: Text
    }
    deriving (Show)

data ObjectResponse
    = Art Artefact
    | Spc Specimen
    deriving (Show)

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

data AgentResponse
    = Prs Person
    | Org Organization
    deriving (Show)

instance FromJSON AgentResponse where
    parseJSON =
        withObject
            "response from /agent endpoint"
            ( \o -> do
                common :: CommonFields <- parseJSON (Data.Aeson.Types.Object o)
                case classLabel common of
                    "Person" -> Prs <$> parseRemainingPerson common o
                    "Organisation" -> Org <$> parseRemainingOrganization common o
                    other ->
                        fail $ "I am expecting a 'type' of 'Person' or 'Organisation' from the /agent endpoint, but I got " <> (Prelude.show other) <> "!"
            )

parseRemainingOrganization :: CommonFields -> Object -> Parser Organization
parseRemainingOrganization cf o =
    Organization cf
        <$> o .: "verbatimBirthDate"
        <*> o .:? "verbatimDeathDate"

parseRemainingPerson :: CommonFields -> Object -> Parser Person
parseRemainingPerson cf o =
    Person cf
        <$> o .: "gender"
        <*> o .: "familyName"
        <*> o .: "givenName"
        <*> o .: "verbatimBirthDate"
        <*> o .:? "verbatimDeathDate"

data Place = Place
    { com :: CommonFields
    , nation :: [Text]
    , location :: Maybe Geolocation
    }
    deriving (Show)

data Geolocation = Geolocation
    { lat :: Float
    , lon :: Float
    }
    deriving (Show)

instance FromJSON Geolocation where
    parseJSON =
        withObject
            "geolocation"
            ( \o ->
                Geolocation
                    <$> o .: "lat"
                    <*> o .: "lon"
            )

instance FromJSON Place where
    parseJSON =
        withObject
            "an object from the /place endpoint"
            ( \o ->
                Place
                    <$> (parseJSON (Data.Aeson.Types.Object o))
                    <*> o .:? "nation" .!= []
                    <*> o .:? "geoLocation"
            )

instance FromJSON Artefact where
    parseJSON = withObject "an object" $ \o -> do
        common :: CommonFields <- parseJSON (Data.Aeson.Types.Object o)
        parseRemainingArtefact common o

instance FromJSON Specimen where
    parseJSON = withObject "an object" $ \o -> do
        common :: CommonFields <- parseJSON (Data.Aeson.Types.Object o)
        parseRemainingSpecimen common o

instance FromJSON Person where
    parseJSON = withObject "an object" $ \o -> do
        common :: CommonFields <- parseJSON (Data.Aeson.Types.Object o)
        parseRemainingPerson common o

instance FromJSON Organization where
    parseJSON = withObject "an object" $ \o -> do
        common :: CommonFields <- parseJSON (Data.Aeson.Types.Object o)
        parseRemainingOrganization common o

data RelatedThings = RelatedThings
    { relatedArtefacts :: [Artefact]
    , relatedSpecimens :: [Specimen]
    , relatedPeople :: [Person]
    , relatedOrgs :: [Organization]
    , relatedPlaces :: [Place]
    }
    deriving (Show)

blankRelatedThings :: RelatedThings
blankRelatedThings =
    RelatedThings
        { relatedArtefacts = []
        , relatedSpecimens = []
        , relatedPeople = []
        , relatedOrgs = []
        , relatedPlaces = []
        }

addOne :: RelatedThings -> Value -> RelatedThings
addOne acc v
    | Just a <- parseMaybe parseJSON v =
        acc{relatedArtefacts = a : relatedArtefacts acc}
    | Just s <- parseMaybe parseJSON v =
        acc{relatedSpecimens = s : relatedSpecimens acc}
    | Just p <- parseMaybe parseJSON v =
        acc{relatedPeople = p : relatedPeople acc}
    | Just o <- parseMaybe parseJSON v =
        acc{relatedOrgs = o : relatedOrgs acc}
    | Just p <- parseMaybe parseJSON v =
        acc{relatedPlaces = p : relatedPlaces acc}
    | otherwise = acc

instance FromJSON RelatedThings where
    parseJSON = withObject "related category response" $ \o ->
        o .: "results" >>= pure . Data.Vector.foldl' addOne blankRelatedThings
