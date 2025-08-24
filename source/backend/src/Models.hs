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
module Models (addReferenceCheckConstraint, migrateAll) where

import Data.Proxy
import Database.Persist.Class.PersistField
import Database.Persist.PersistValue (PersistValue(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlPersistT, rawExecute)
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
    = RAgent
    | RConcept
    | RDocument
    | RFieldCollection
    | RGroup
    | RObject
    | RPlace
    | RTaxon
    | RTopic
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


Person

Organization

Collaboration

Category

Publication

FieldCollection

Group

Artefact
    title String
    collection ArtefactCollection

Specimen
    title String
    collection SpecimenCollection

Place

Taxon

Topic
|]

addReferenceCheckConstraint :: SqlPersistT IO ()
addReferenceCheckConstraint = rawExecute
    "ALTER TABLE reference \
    \ADD CONSTRAINT only_one_cache_present \
    \CHECK ( \
        \ (CASE WHEN cached_person IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_organization IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_collaboration IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_category IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_publication IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_field_collection IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_group IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_artefact IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_specimen IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_place IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_taxon IS NOT NULL THEN 1 ELSE 0 END) + \
        \ (CASE WHEN cached_topic IS NOT NULL THEN 1 ELSE 0 END) <= 1 \
    \);"
    []
