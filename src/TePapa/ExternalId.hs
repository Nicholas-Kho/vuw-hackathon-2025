{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.ExternalId (
    ExternalId (..),
    MuseumResource (..),
    TePapaReference (..),
    parseReferenceyObject,
    showTePapaReferenceNice,
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import Data.Text
import GHC.Generics

data MuseumResource
    = ObjectR
    | AgentR
    | PlaceR
    | ConceptR
    | TopicR
    deriving (Show, Eq, Ord, Generic)

instance Hashable MuseumResource

instance FromJSON MuseumResource where
    parseJSON = withText "a museum resource class label" $ \case
        "Object" -> pure ObjectR
        "Specimen" -> pure ObjectR
        "Person" -> pure AgentR
        "Organisation" -> pure AgentR
        "Place" -> pure PlaceR
        "Category" -> pure ConceptR
        "Topic" -> pure TopicR
        other -> fail $ "I can't map " <> (Prelude.show other) <> " to a museum resource type."

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

parseReferenceyObject :: Value -> Parser (TePapaReference, Text)
parseReferenceyObject =
    withObject
        "Referencey object"
        ( \o -> do
            typ <- o .: "type"
            eid <- o .: "id"
            linkTitle <- o .: "title"
            pure $ (TePapaReference typ eid, linkTitle)
        )
