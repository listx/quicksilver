{-# LANGUAGE RecordWildCards #-}
module Data.Recruit where

import Util

data RecruitPoolM2TW = RecruitPoolM2TW
    { unitName :: String
    , pointsInitial :: Int -- starting point (units available upon completion of building)
    , pointsGrowth :: Double -- points gained per turn (set to 0.000001 by Creative Assembly to only allow retraining, not recruiting)
    , pointsCap :: Double -- maximum build points possible (set to 0.999 by Creative Assembly to only allow retraining, not recruiting)
    , experience :: Int -- starting experience of the unit (0 to 9)
    , req :: String -- required things (e.g., "requires factions { england, scotland...")
    }

instance Show RecruitPoolM2TW where
    show RecruitPoolM2TW{..} = "                recruit_pool "
        ++ dquote unitName ++ " "
        ++ show pointsInitial ++ " "
        ++ showD pointsGrowth ++ " "
        ++ (if pointsCap /= 0.999 then show else showD) pointsCap ++ " "
        ++ show experience ++ " "
        ++ req

data BuildingM2TW
    = Core
    | Castle
    | Equestrian
    | Barracks
    | CastleBarracks
    | ProfessionalMilitary
    | Missiles
    | Siege
    | Cannon
    | UrbanEquestrian
    | Port
    | Taverns
    | CityHall
    | Bullring
    | Caravan
    | AssassinsGuild
    | AssassinsMuslimGuild
    | MasonsGuild
    | MerchantsGuild
    | TemplarsChapterHouse
    | StJohnsChapterHouse
    | TeutonicKnightsChapterHouse
    | KnightsOfSantiagoChapterHouse
    | WoodsmensGuild
    -- there are other buildings as well, but we only concern ourselves with
    -- those buildings that give unit recruitment abilities, for now
    deriving (Eq)

modRecruit :: (Int, Double, Int, Int) -> RecruitPoolM2TW -> String
modRecruit (p, g, c ,e) RecruitPoolM2TW{..} = show RecruitPoolM2TW
    { unitName = unitName
    , pointsInitial = pointsInitial + p
    , pointsGrowth = if pointsGrowth /= 0.000001 then pointsGrowth + g else pointsGrowth
    , pointsCap = if pointsCap /= 0.999 then pointsCap + (fromIntegral c) else pointsCap
    , experience = experience + e
    , req = req
    }
