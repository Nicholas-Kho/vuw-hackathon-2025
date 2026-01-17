{-# LANGUAGE OverloadedRecordDot #-}

module TePapa.CommonObject (
    TePapaThing (..),
    fromAgentResponse,
    fromCategory,
    fromObjectResponse,
    fromPlace,
    fromRelated,
    fromTopic,
    getCommon,
    getId,
    prettyPrintThing,
) where

import qualified Data.Text as T
import TePapa.Decode

data TePapaThing
    = APerson !Person
    | AnOrg !Organization
    | AnArtefact !Artefact
    | ASpecimen !Specimen
    | APlace !Place
    | ACategory !Category
    | ATopic !Topic

prettyPrintThing :: TePapaThing -> String
prettyPrintThing thing = T.unpack (getCommon thing).title

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

fromRelated :: RelatedThings -> [TePapaThing]
fromRelated
    RelatedThings
        { relatedSpecimens = rs
        , relatedPlaces = rps
        , relatedPeople = rpl
        , relatedOrgs = ro
        , relatedArtefacts = ra
        } =
        concat
            [ AnArtefact <$> ra
            , ASpecimen <$> rs
            , APerson <$> rpl
            , AnOrg <$> ro
            , APlace <$> rps
            ]

getId :: TePapaThing -> TePapaReference
getId thing =
    let
        thingEid = (getCommon thing).eid
     in
        case thing of
            APerson _ -> TePapaReference{namespace = AgentR, eid = thingEid}
            AnOrg _ -> TePapaReference{namespace = AgentR, eid = thingEid}
            AnArtefact _ -> TePapaReference{namespace = ObjectR, eid = thingEid}
            ASpecimen _ -> TePapaReference{namespace = ObjectR, eid = thingEid}
            APlace _ -> TePapaReference{namespace = PlaceR, eid = thingEid}
            ACategory _ -> TePapaReference{namespace = ConceptR, eid = thingEid}
            ATopic _ -> TePapaReference{namespace = TopicR, eid = thingEid}
