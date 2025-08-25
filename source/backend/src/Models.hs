{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
module Models (addReferenceCheckConstraint, doMigrations) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import Data.Time.Clock (UTCTime)
import Database.Persist.Class.PersistField
import Database.Persist.PersistValue (PersistValue(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlPersistT, SqlBackend, rawExecute, runMigration)
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Database.Persist.Types (SqlType(SqlString))
import qualified Data.Text as T

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
    fromPersistText  _ = Left $ typename (Proxy @a) <> " is represented as text."
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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Want a polymorphic association between "objects" and other "objects"
-- ASSUMPTION: AT MOST ONE of the 'cached' fields are not null.
-- we have "addReferenceCheckConstraint" to do this. (run after migrations.)
Reference
    to_type MuseumResource
    external_id Int
    cached_person PersonId Maybe
    cached_organization OrganizationId Maybe
    cached_collaboration CollaborationId Maybe
    cached_category CategoryId Maybe
    cached_publication PublicationId Maybe
    cached_field_collection FieldCollectionId Maybe
    cached_group GroupId Maybe
    cached_artefact ArtefactId Maybe
    cached_specimen SpecimenId Maybe
    cached_place PlaceId Maybe
    cached_taxon TaxonId Maybe
    cached_topic TopicId Maybe

    UniqueReference to_type external_id

CacheMetadata
    addedAt UTCTime
    ttlSeconds Int default=1200
    reference ReferenceId

Person
    title String
    associations AgentAssociationsId

Organization
    title String
    associations AgentAssociationsId

Collaboration
    title String
    associations AgentAssociationsId

-- Associations for people, organizations, and collaborations.
AgentAssociations
    --Identifier of a Specimen
    identification_identified[ReferenceId] 
    --Maker of an Object
    production_contributor[ReferenceId] 
    --Associated with another agent
    associated_parties [ReferenceId]
    --Author of a Topic or Publication
    authors [ReferenceId]
    --Recorder of the field collection of a Specimen
    evidence_for_at_event_recorded_by [ReferenceId]
    --Author of the scientific name of a Specimen
    scientific_name_authorship [ReferenceId]
    --Associated with another agent, Category, or Place
    associated_with [ReferenceId]
    --Depicted in an Object
    depicts [ReferenceId]
    --Former owner of an Object
    former_owner [ReferenceId]
    --Referred to by an Object, Specimen, Topic, or Publication
    refers_to [ReferenceId]
    --Influenced the making of an Object
    influenced_by [ReferenceId]
    --Publisher of a Publication
    publisher [ReferenceId]
    --Editor of a Publication
    editor [ReferenceId]
    --Illustrator of a Publication
    illustrator [ReferenceId]
    --Part of an aggregation of agents
    aggregated_agents [ReferenceId]
    --Associated in an unspecified way with an Object or Specimen 
    unknown_association [ReferenceId]

Category
    title String
    --Associated with another category, Place, Person, Organisation, or Collaboration
    associated_with [ReferenceId]
    --Concept depicted in an Object
    depicts [ReferenceId]
    --Style/group/etc. referred to by an Object, Specimen, Topic, or Publication
    refers_to [ReferenceId]
    --Subject of an Object
    is_about [ReferenceId]
    --Style/group/etc. that influenced the making of an Object
    influenced_by [ReferenceId]
    --Subject that an Object is intended for
    intended_for [ReferenceId]
    --Technique used in the making of an Object
    production_used_technique [ReferenceId]
    --Object type of an Object
    is_type_of [ReferenceId]
    --Material that an Object is made of
    is_made_of [ReferenceId]
    --Associated in an unspecified way with an Object or Specimen
    unknown_association [ReferenceId]
    --Child category underneath a parent Category 
    related_terms [ReferenceId]

Publication
    title String
    --Referred to by an Object or Specimen
    is_referenced_by [ReferenceId] 
    --Associated with a Topic
    related_topics [ReferenceId] 
    --Parent of one or more other child Publications
    is_part_of [ReferenceId] 
    --Child publication underneath a parent Publication 
    has_part [ReferenceId] 

FieldCollection
    title String
    evidence_for_at_event [ReferenceId]

Group
    title String
    aggregatedGroups [ReferenceId]

Artefact
    title String
    collection ArtefactCollection
    associations ObjectAssociationsId

Specimen
    title String
    collection SpecimenCollection
    associations ObjectAssociationsId

-- Associations for Artefacts and Specimens
ObjectAssociations
    --Associated with another Object or Specimen
    relation [ReferenceId] 
    --Related to a Topic or Publication
    related_objects [ReferenceId] 
    --Parent of one or more other children Objects or Specimens
    is_part_of [ReferenceId] 
    --Child object or specimen underneath a parent Object or Specimen
    has_part [ReferenceId] 
    --Part of an aggregation of Objects or Specimens 
    aggregated_objects [ReferenceId] 

Place
    title String
    --Place related to where an Object was made
    production_spatial [ReferenceId]
    --Associated with another place, Category, Person, Organisation, or Collaboration
    associated_with [ReferenceId]
    --Concept depicted in an Object
    depicts [ReferenceId]
    --Place referred to by an Object, Specimen, Topic, or Publication
    refers_to [ReferenceId]
    --Subject of an Object
    is_about [ReferenceId]
    --Place that influenced the making of an Object
    influenced_by [ReferenceId]
    --Child place within a parent Place 
    related_terms [ReferenceId]



Taxon
    title String
    -- Associated with a Publication or Topic
    associated_with [ReferenceId]
    -- Taxon depicted in an Object
    depicts [ReferenceId]
    -- Taxon referred to by an Object, Specimen, Topic, or Publication
    refers_to [ReferenceId]
    -- Subject of an Object
    is_about [ReferenceId]
    -- Taxon that influenced the making of an Object
    influenced_by [ReferenceId]
    -- Subject that an Object is intended for
    intended_for [ReferenceId]
    -- Associated in an unspecified way with an Object or Specimen 
    unknown_association [ReferenceId]

Topic
    title String
    -- Referred to by an Object or Specimen
    is_referenced_by [ReferenceId]
    -- Associated with a Topic
    related_topics [ReferenceId]
    -- Parent of one or more other child Topics
    is_part_of [ReferenceId]
    -- Child topic underneath a parent Topic
    has_part [ReferenceId]
    -- Part of an aggregation of Topics 
    aggregated_topics [ReferenceId]
|]

addReferenceCheckConstraint :: MonadIO m => SqlPersistT m ()
addReferenceCheckConstraint = do
  -- Reusable condition: count how many cached_* columns are non-null
  let cond =
        "( " <>
        "  (CASE WHEN NEW.cached_person IS NOT NULL THEN 1 ELSE 0 END) + "  <>
        "  (CASE WHEN NEW.cached_organization IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_collaboration IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_category IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_publication IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_field_collection IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_group IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_artefact IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_specimen IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_place IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_taxon IS NOT NULL THEN 1 ELSE 0 END) + " <>
        "  (CASE WHEN NEW.cached_topic IS NOT NULL THEN 1 ELSE 0 END) "  <>
        ") > 1"

  -- Drop old triggers so this is idempotent
  rawExecute "DROP TRIGGER IF EXISTS only_one_cache_present_ins;" []
  rawExecute "DROP TRIGGER IF EXISTS only_one_cache_present_upd;" []

  -- INSERT trigger
  rawExecute
    ( "CREATE TRIGGER only_one_cache_present_ins "
   <> "BEFORE INSERT ON reference "
   <> "FOR EACH ROW "
   <> "WHEN " <> cond <> " "
   <> "BEGIN "
   <> "  SELECT RAISE(FAIL, 'only one cache field may be non-null'); "
   <> "END;"
    ) []

  -- UPDATE trigger (fires only if one of the cached_* columns is updated)
  rawExecute
    ( "CREATE TRIGGER only_one_cache_present_upd "
   <> "BEFORE UPDATE OF "
   <> "cached_person, cached_organization, cached_collaboration, cached_category, "
   <> "cached_publication, cached_field_collection, cached_group, cached_artefact, "
   <> "cached_specimen, cached_place, cached_taxon, cached_topic "
   <> "ON reference "
   <> "FOR EACH ROW "
   <> "WHEN " <> cond <> " "
   <> "BEGIN "
   <> "  SELECT RAISE(FAIL, 'only one cache field may be non-null'); "
   <> "END;"
    ) []



doMigrations :: MonadIO m => SqlPersistT m ()
doMigrations = runMigration migrateAll
