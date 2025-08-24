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
module Models () where

import Data.Proxy
import Database.Persist.Class.PersistField
import Database.Persist.PersistValue (PersistValue(..))
import Database.Persist.Sql (PersistFieldSql(..))
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

instance PersistField ArtefactCollection where
    toPersistValue = PersistText . toText
    fromPersistValue = fromPersistText

instance PersistFieldSql ArtefactCollection where
    sqlType _ = SqlString

instance PersistField SpecimenCollection where
    toPersistValue = PersistText . toText
    fromPersistValue = fromPersistText

instance PersistFieldSql SpecimenCollection where
    sqlType _ = SqlString


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Artefact
    title String
    collection ArtefactCollection

Specimen
    title String
    collection SpecimenCollection
|]
