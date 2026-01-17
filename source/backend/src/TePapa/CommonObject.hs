{-# LANGUAGE OverloadedRecordDot #-}

module TePapa.CommonObject (
    TePapaThing (..),
    fromAgentResponse,
    fromCategory,
    fromObjectResponse,
    fromPlace,
    fromTopic,
    getCommon,
) where

import TePapa.Decode

data TePapaThing
    = APerson !Person
    | AnOrg !Organization
    | AnArtefact !Artefact
    | ASpecimen !Specimen
    | APlace !Place
    | ACategory !Category
    | ATopic !Topic

getCommon :: TePapaThing -> CommonFields
getCommon t = case t of
    APerson p -> p.com
    AnOrg o -> o.com
    AnArtefact a -> a.com
    ASpecimen s -> s.com
    APlace p -> p.com
    ACategory c -> c.com
    ATopic tp -> tp.com

fromObjectResponse :: ObjectResponse -> TePapaThing
fromObjectResponse resp =
    case resp of
        Art a -> AnArtefact a
        Spc s -> ASpecimen s

fromAgentResponse :: AgentResponse -> TePapaThing
fromAgentResponse resp =
    case resp of
        Prs p -> APerson p
        Org g -> AnOrg g

fromPlace :: Place -> TePapaThing
fromPlace = APlace

fromCategory :: Category -> TePapaThing
fromCategory = ACategory

fromTopic :: Topic -> TePapaThing
fromTopic = ATopic
