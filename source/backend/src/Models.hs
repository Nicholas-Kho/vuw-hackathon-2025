{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Models (
    CacheType (..),
    MuseumResource (..),
    Person (..),
    Collaboration (..),
    Organization (..),
    doMigrations,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import qualified Data.Text as T
import Database.Persist.Class.PersistField
import Database.Persist.PersistValue (PersistValue (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlPersistT, runMigration)
import Database.Persist.TH (derivePersistField, mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (Key, SqlType (SqlString))

data ArtefactCollection
    = Art
    | History
    | PacificCultures
    | Philatelic
    | Photography
    | TaongaMaori
    | RareBooks
    | CollectedArchives
    | MuseumArchives
    deriving (Show, Read, Eq)

data SpecimenCollection
    = Archaeozoology
    | Birds
    | Fish
    | FossilVertebrates
    | Geology
    | Crustacea
    | Molluscs
    | MarineInvertebrates
    | MarineMammals
    | LandMammals
    | Plants
    | ReptilesAndAmphibians
    | Insects
    deriving (Show, Read, Eq)

data MuseumResource
    = MRAgent
    | MRConcept
    | MRDocument
    | MRFieldCollection
    | MRGroup
    | MRObject
    | MRPlace
    | MRTaxon
    | MRTopic
    deriving (Show, Read, Eq)

-- Note: I tried to make generic instances of PersistField and PersistFieldSql by
-- "instance (PersistEnum a) => PersistField a where ...", but this results in some nasty conflicts
-- with Persistent's key types. I've kept this class for now, but later if there's more enums, we can
-- try to tackle this error again, or use a different solution.
class (Show a, Read a) => PersistEnum a where
    typename :: Proxy a -> T.Text
    fromPersistText :: PersistValue -> Either T.Text a
    fromPersistText (PersistText t) =
        case reads (T.unpack t) of
            [(v, "")] -> Right v
            _ -> Left $ "Text must correspond to " <> typename (Proxy @a) <> " type."
    fromPersistText _ = Left $ typename (Proxy @a) <> " is represented as text."
    toText :: a -> T.Text
    toText = T.pack . show

instance PersistEnum ArtefactCollection where
    typename _ = "ArtefactCollection"

instance PersistEnum SpecimenCollection where
    typename _ = "SpecimenCollection"

instance PersistEnum MuseumResource where
    typename _ = "MuseumObject"

instance PersistField ArtefactCollection where
    toPersistValue = PersistText . toText
    fromPersistValue = fromPersistText

instance PersistField SpecimenCollection where
    toPersistValue = PersistText . toText
    fromPersistValue = fromPersistText

instance PersistField MuseumResource where
    toPersistValue = PersistText . toText
    fromPersistValue = fromPersistText

instance PersistFieldSql ArtefactCollection where
    sqlType _ = SqlString

instance PersistFieldSql SpecimenCollection where
    sqlType _ = SqlString

instance PersistFieldSql MuseumResource where
    sqlType _ = SqlString

-- A Reference to a Tepapa Collections API object.
data TORef = MkRef
    { resourceType :: MuseumResource
    , externalId :: Int
    }
    deriving (Eq, Read, Show)

$(derivePersistField "TORef")

-- Types of associations Agent resources (people, organizations, collaborations) can have:
data AgentAssociations = MkAgentAssoc
    { -- Identifier of a Specimen
      identificationIdentified :: [TORef]
    , -- Maker of an Object
      productionContributor :: [TORef]
    , -- Associated with another agent
      associatedParties :: [TORef]
    , -- Author of a Topic or Publication
      authors :: [TORef]
    , -- Recorder of the field collection of a Specimen
      evidenceForAtEventRecordedBy :: [TORef]
    , -- Author of the scientific name of a Specimen
      scientificNameAuthorship :: [TORef]
    , -- Associated with another agent, Category, or Place
      associatedWith :: [TORef]
    , -- Depicted in an Object
      depicts :: [TORef]
    , -- Former owner of an Object
      formerOwner :: [TORef]
    , -- Referred to by an Object, Specimen, Topic, or Publication
      refersTo :: [TORef]
    , -- Influenced the making of an Object
      influencedBy :: [TORef]
    , -- Publisher of a Publication
      publisher :: [TORef]
    , -- Editor of a Publication
      editor :: [TORef]
    , -- Illustrator of a Publication
      illustrator :: [TORef]
    , -- Part of an aggregation of agents
      aggregatedAgents :: [TORef]
    , -- Associated in an unspecified way with an Object or Specimen
      unknownAssociation :: [TORef]
    }
    deriving (Eq, Read, Show)

$(derivePersistField "AgentAssociations")

-- Associations an Object (artefact, specimen) can have
data ObjectAssociations = MkObjectAssoc
    { -- Associated with another Object or Specimen
      relation :: [TORef]
    , -- Related to a Topic or Publication
      related_objects :: [TORef]
    , -- Parent of one or more other children Objects or Specimens
      is_part_of :: [TORef]
    , -- Child object or specimen underneath a parent Object or Specimen
      has_part :: [TORef]
    , -- Part of an aggregation of Objects or Specimens
      aggregated_objects :: [TORef]
    }
    deriving (Eq, Read, Show)

$(derivePersistField "ObjectAssociations")

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|

Person
    external_id Int
    title String
    verbatim_birth_date String
    verbatim_death_date String
    bith_place String
    death_place String
    associations AgentAssociations

Organization
    external_id Int
    title String
    associations AgentAssociations

Collaboration
    external_id Int
    title String
    associations AgentAssociations

Category
    title String
    external_id Int
    --Associated with another category, Place, Person, Organisation, or Collaboration
    associated_with [TORef]
    --Concept depicted in an Object
    depicts [TORef]
    --Style/group/etc. referred to by an Object, Specimen, Topic, or Publication
    refers_to [TORef]
    --Subject of an Object
    is_about [TORef]
    --Style/group/etc. that influenced the making of an Object
    influenced_by [TORef]
    --Subject that an Object is intended for
    intended_for [TORef]
    --Technique used in the making of an Object
    production_used_technique [TORef]
    --Object type of an Object
    is_type_of [TORef]
    --Material that an Object is made of
    is_made_of [TORef]
    --Associated in an unspecified way with an Object or Specimen
    unknown_association [TORef]
    --Child category underneath a parent Category 
    related_terms [TORef]

Publication
    title String
    external_id Int
    --Referred to by an Object or Specimen
    is_referenced_by [TORef] 
    --Associated with a Topic
    related_topics [TORef] 
    --Parent of one or more other child Publications
    is_part_of [TORef] 
    --Child publication underneath a parent Publication 
    has_part [TORef] 

FieldCollection
    title String
    external_id Int
    evidence_for_at_event [TORef]

Group
    title String
    external_id Int
    aggregatedGroups [TORef]

Artefact
    title String
    external_id Int
    collection ArtefactCollection
    associations ObjectAssociations

Specimen
    title String
    external_id Int
    collection SpecimenCollection
    associations ObjectAssociations

Place
    title String
    external_id Int
    --Place related to where an Object was made
    production_spatial [TORef]
    --Associated with another place, Category, Person, Organisation, or Collaboration
    associated_with [TORef]
    --Concept depicted in an Object
    depicts [TORef]
    --Place referred to by an Object, Specimen, Topic, or Publication
    refers_to [TORef]
    --Subject of an Object
    is_about [TORef]
    --Place that influenced the making of an Object
    influenced_by [TORef]
    --Child place within a parent Place 
    related_terms [TORef]



Taxon
    title String
    external_id Int
    -- Associated with a Publication or Topic
    associated_with [TORef]
    -- Taxon depicted in an Object
    depicts [TORef]
    -- Taxon referred to by an Object, Specimen, Topic, or Publication
    refers_to [TORef]
    -- Subject of an Object
    is_about [TORef]
    -- Taxon that influenced the making of an Object
    influenced_by [TORef]
    -- Subject that an Object is intended for
    intended_for [TORef]
    -- Associated in an unspecified way with an Object or Specimen 
    unknown_association [TORef]

Topic
    title String
    external_id Int
    -- Referred to by an Object or Specimen
    is_referenced_by [TORef]
    -- Associated with a Topic
    related_topics [TORef]
    -- Parent of one or more other child Topics
    is_part_of [TORef]
    -- Child topic underneath a parent Topic
    has_part [TORef]
    -- Part of an aggregation of Topics 
    aggregated_topics [TORef]
|]

data CacheType
    = Miss Int MuseumResource
    | HitPerson (Key Person)
    | HitOrganization (Key Organization)
    | HitCollaboration (Key Collaboration)
    | HitCategory (Key Category)
    | HitPublication (Key Publication)
    | HitFieldCollection (Key FieldCollection)
    | HitGroup (Key Group)
    | HitArtefact (Key Artefact)
    | HitSpecimen (Key Specimen)
    | HitPlace (Key Place)
    | HitTaxon (Key Taxon)
    | HitTopic (Key Topic)

doMigrations :: (MonadIO m) => SqlPersistT m ()
doMigrations = runMigration migrateAll
