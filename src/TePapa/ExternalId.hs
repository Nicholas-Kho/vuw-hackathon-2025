{-# LANGUAGE OverloadedRecordDot #-}

module TePapa.ExternalId (
    ExternalId (..),
    MuseumResource (..),
    TePapaReference (..),
    showTePapaReferenceNice,
) where

import Data.Aeson
import Data.Hashable
import GHC.Generics

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
