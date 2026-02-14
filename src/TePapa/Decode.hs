{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Decode (
    AgentResponse (..),
    Person (..),
    Artefact (..),
    Category (..),
    CommonFields (..),
    RelatedThings (..),
    Specimen (..),
    Organization (..),
    ObjectResponse (..),
    Place (..),
    Topic (..),
    Geolocation (..),
)
where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.Vector
import GHC.Generics
import TePapa.Association (Association, parseCommonOutgoingEdges)
import TePapa.ExternalId

data CommonFields = CommonFields
    { eid :: ExternalId
    , title :: Text
    , classLabel :: Text
    , outgoing :: [Association]
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

data Category = Category
    { com :: CommonFields
    }

instance FromJSON Category where
    parseJSON = withObject "a category" $ \o ->
        Category <$> parseJSON (Data.Aeson.Types.Object o)

data Topic = Topic
    { com :: CommonFields
    , narrative :: Text
    }

instance FromJSON Topic where
    parseJSON = withObject "a topic" $ \o ->
        Topic
            <$> parseJSON (Data.Aeson.Types.Object o)
            <*> o .: "narrative"
