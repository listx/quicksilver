{-# LANGUAGE RecordWildCards #-}

module Data.Recruit.M2TW where

import Util

data RecruitPool = RecruitPool
    { unitName :: String
    , pointsInitial :: Int -- starting point (units available upon completion of building)
    , pointsGrowth :: Double -- points gained per turn (set to 0.000001 by Creative Assembly to only allow retraining, not recruiting)
    , pointsCap :: Double -- maximum build points possible (set to 0.999 by Creative Assembly to only allow retraining, not recruiting)
    , experience :: Int -- starting experience of the unit (0 to 9)
    , req :: String -- required things (e.g., "requires factions { england, scotland...")
    }

instance Show RecruitPool where
    show RecruitPool{..} = "                recruit_pool "
        ++ dquote unitName ++ " "
        ++ show pointsInitial ++ " "
        ++ showD pointsGrowth ++ " "
        ++ (if pointsCap /= 0.999 then show else showD) pointsCap ++ " "
        ++ show experience ++ " "
        ++ req

data Building
    = Core
    | Castle
    | Equestrian
    | Barracks
    | CastleBarracks
    | ProfessionalMilitary
    | Missiles
    | Siege
    | CastleSiege
    | Cannon
    | CastleCannon
    | UrbanEquestrian
    | Port
    | CastlePort
    | Tavern
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

instance Show Building where
    show b = case b of
        Core                          -> "core_building"
        Castle                        -> "core_castle_building"
        Equestrian                    -> "equestrian"
        Barracks                      -> "barracks"
        CastleBarracks                -> "castle_barracks"
        ProfessionalMilitary          -> "professional_military"
        Missiles                      -> "missiles"
        Siege                         -> "siege"
        CastleSiege                   -> "castle_siege"
        Cannon                        -> "cannon"
        CastleCannon                  -> "castle_cannon"
        UrbanEquestrian               -> "urban_equestrian"
        Port                          -> "port"
        CastlePort                    -> "castle_port"
        Tavern                        -> "tavern"
        CityHall                      -> "city_hall"
        Bullring                      -> "bullring"
        Caravan                       -> "caravan"
        AssassinsGuild                -> "guild_assassins_guild"
        AssassinsMuslimGuild          -> "guild_assassins_muslim_guild"
        MasonsGuild                   -> "guild_masons_guild"
        MerchantsGuild                -> "guild_merchants_guild"
        TemplarsChapterHouse          -> "guild_templars_chapter_house"
        StJohnsChapterHouse           -> "guild_st_johns_chapter_house"
        TeutonicKnightsChapterHouse   -> "guild_teutonic_knights_chapter_house"
        KnightsOfSantiagoChapterHouse -> "guild_knights_of_santiago_chapter_house"
        _                             -> "guild_woodsmens_guild"


village, town, largeTown, city, largeCity, hugeCity1, hugeCity2 :: (Int, Double, Int, Int)
village = (0, 0.0, 0, 0) -- town
town = (1, 0.10, 1, 1) -- town
largeTown = (2, 0.10, 2, 2) -- large_town
city = (3, 0.15, 3, 3) -- city ("Minor City" in-game)
largeCity = (4, 0.20, 4, 5) -- large_city
hugeCity1 = (6, 0.30, 6, 7) -- huge_city
hugeCity2 = (7, 0.35, 7, 7) -- huge_city

-- recruitment:
--      recruit_pool
--      UNIT_TYPE
--    b starting point (units available upon completion of building)
--    g points gained per turn
--    c maximum points possible
--    e starting experience of the unit (0 to 9)
--      restrict to certain factions

modRecruit :: (Int, Double, Int, Int) -> RecruitPool -> String
modRecruit (p, g, c ,e) RecruitPool{..} = show RecruitPool
    { unitName = unitName
    , pointsInitial = pointsInitial + p
    , pointsGrowth = if pointsGrowth /= 0.000001 then pointsGrowth + g else pointsGrowth
    , pointsCap = if pointsCap /= 0.999 then pointsCap + (fromIntegral c) else pointsCap
    , experience = experience + e
    , req = req
    }

getRecruits :: Building -> (Int, Double, Int, Int) -> String
getRecruits btype rmod = concatMap (\s -> modRecruit rmod s ++ "\r\n") (map snd . filter (\(a, _) -> a == btype) $ _RECRUIT_DATA)

_RECRUIT_DATA :: [(Building, RecruitPool)]
_RECRUIT_DATA =
    zip (repeat Core)
    [ RecruitPool "Conquistadores" 1 0.5 4 1 "requires factions { spain, portugal, }  and hidden_resource america"
    , RecruitPool "Swiss Guard" 1 0.5 4 0 "requires factions { papal_states, }"
    , RecruitPool "Hussars" 1 0.5 4 0 "requires factions { poland, hungary, }"
    , RecruitPool "Cossack Musketeers" 1 0.5 4 0 "requires factions { russia, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Dismounted Conquistadores" 1 0.5 4 1 "requires factions { spain, portugal, }  and hidden_resource america"
    , RecruitPool "Town Militia" 1 0.2 2 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, Normans, }"
    , RecruitPool "Italian Militia" 1 0.2 2 0 "requires factions { milan, venice, papal_states, sicily, }"
    , RecruitPool "EE Town Militia" 1 0.2 2 0 "requires factions { poland, hungary, }"
    , RecruitPool "EE Archer Militia" 1 0.2 2 0 "requires factions { russia, }"
    , RecruitPool "SE Town Militia" 1 0.2 2 0 "requires factions { byzantium, }"
    , RecruitPool "ME Town Militia" 1 0.2 2 0 "requires factions { moors, turks, mongols, timurids, }"
    , RecruitPool "ME Archer Militia" 1 0.2 2 0 "requires factions { egypt, }"
    , RecruitPool "Peasant Spearmen" 1 0.2 2 0 "requires factions { Saxons, }"
    , RecruitPool "Spear Militia" 0 0.000001 0.999 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, Normans, }"
    , RecruitPool "Italian Spear Militia" 0 0.000001 0.999 0 "requires factions { milan, venice, papal_states, sicily, }"
    , RecruitPool "EE Spear Militia" 0 0.000001 0.999 0 "requires factions { poland, russia, hungary, }"
    , RecruitPool "SE Spear Militia" 0 0.000001 0.999 0 "requires factions { byzantium, }"
    , RecruitPool "ME Spear Militia" 0 0.000001 0.999 0 "requires factions { moors, egypt, turks, mongols, timurids, }"
    , RecruitPool "Archer Militia" 0 0.000001 0.999 0 "requires factions { england, }"
    , RecruitPool "Scots Pike Militia" 0 0.000001 0.999 0 "requires factions { scotland, }"
    , RecruitPool "Crossbow Militia" 0 0.000001 0.999 0 "requires factions { france, hre, denmark, spain, portugal, }"
    , RecruitPool "Genoese Crossbow Militia" 0 0.000001 0.999 0 "requires factions { milan, }"
    , RecruitPool "Pavise Crossbow Militia" 0 0.000001 0.999 0 "requires factions { venice, papal_states, sicily, hungary, }"
    , RecruitPool "EE Crossbow Militia" 0 0.000001 0.999 0 "requires factions { poland, russia, }"
    , RecruitPool "S Archer Militia" 0 0.000001 0.999 0 "requires factions { byzantium, }"
    , RecruitPool "ME Crossbow Militia" 0 0.000001 0.999 0 "requires factions { moors, }"
    , RecruitPool "Saracen Militia" 0 0.000001 0.999 0 "requires factions { egypt, turks, }"
    , RecruitPool "Sabadar Militia" 0 0.000001 0.999 0 "requires factions { timurids, }"
    , RecruitPool "Mongol Horse Archers" 1 0.4 3 0 "requires factions { mongols, }"
    , RecruitPool "Mounted Sergeants" 1 0.4 3 0 "requires factions { Normans, }"
    , RecruitPool "Theigns" 1 0.4 3 0 "requires factions { Saxons, }"
    , RecruitPool "Dismounted Broken Lances" 1 0.4 3 0 "requires factions { milan, }"
    , RecruitPool "Noble Pikemen" 1 0.4 3 0 "requires factions { scotland, }"
    , RecruitPool "Scots Guard" 1 0.4 3 0 "requires factions { france, }"
    , RecruitPool "Christian Guard" 1 0.4 3 0 "requires factions { moors, }"
    , RecruitPool "Broken Lances" 1 0.5 4 0 "requires factions { milan, venice, papal_states, }"
    ]
    ++ zip (repeat Castle)
    [ RecruitPool "Dismounted Polish Knights" 1 0.4 3 0 "requires factions { poland, }"
    , RecruitPool "Khan's Guard" 1 0.4 3 0 "requires factions { mongols, timurids, }"
    , RecruitPool "Noble Swordsmen" 1 0.4 3 0 "requires factions { scotland, }"
    , RecruitPool "Noble Knights" 1 0.4 3 0 "requires factions { france, }"
    , RecruitPool "Imperial Knights" 1 0.4 3 0 "requires factions { hre, }"
    , RecruitPool "Chivalric Knights" 1 0.4 3 0 "requires factions { spain, sicily, }"
    , RecruitPool "Italian MAA" 1 0.4 3 0 "requires factions { milan, venice, papal_states, }"
    , RecruitPool "Polish Retainers" 1 0.4 3 0 "requires factions { poland, }"
    , RecruitPool "E Chivalric Knights" 1 0.4 3 0 "requires factions { hungary, }"
    , RecruitPool "Byzantine Lancers" 1 0.4 3 0 "requires factions { byzantium, }"
    , RecruitPool "Granadine Lancers" 1 0.4 3 0 "requires factions { moors, }"
    , RecruitPool "Mongol Heavy Lancers" 1 0.4 3 0 "requires factions { mongols, timurids, }"
    , RecruitPool "Feudal Knights" 1 0.4 3 0 "requires factions { scotland, denmark, }"
    , RecruitPool "Polish Knights" 1 0.4 3 0 "requires factions { poland, }"
    , RecruitPool "Boyar Sons" 1 0.4 3 0 "requires factions { russia, }"
    , RecruitPool "Vardariotai" 1 0.4 3 0 "requires factions { byzantium, }"
    , RecruitPool "Granadine Jinetes" 1 0.4 3 0 "requires factions { moors, }"
    , RecruitPool "Mamluks" 1 0.4 3 0 "requires factions { egypt, }"
    , RecruitPool "Sipahi Lancers" 1 0.4 3 0 "requires factions { turks, }"
    , RecruitPool "Mongol Heavy Archers" 1 0.4 3 0 "requires factions { mongols, timurids, }"
    , RecruitPool "Dismounted Polish Nobles" 1 0.4 3 0 "requires factions { poland, }"
    , RecruitPool "Mailed Knights" 1 0.4 3 0 "requires factions { Normans, }"
    , RecruitPool "English Huscarls" 1 0.4 3 0 "requires factions { Saxons, }"
    , RecruitPool "Hobilars" 1 0.4 3 0 "requires factions { england, }"
    , RecruitPool "Border Horse" 1 0.4 3 0 "requires factions { scotland, }"
    , RecruitPool "Mounted Sergeants" 1 0.4 3 0 "requires factions { france, hre, milan, venice, papal_states, sicily, Normans, }"
    , RecruitPool "Scouts" 1 0.4 3 0 "requires factions { denmark, }"
    , RecruitPool "Jinetes" 1 0.4 3 0 "requires factions { spain, portugal, }"
    , RecruitPool "Polish Shooters" 1 0.4 3 0 "requires factions { poland, }"
    , RecruitPool "Kazaks" 1 0.4 3 0 "requires factions { russia, }"
    , RecruitPool "Magyar Cavalry" 1 0.4 3 0 "requires factions { hungary, }"
    , RecruitPool "Skythikon" 1 0.4 3 0 "requires factions { byzantium, }"
    , RecruitPool "Desert Cavalry" 1 0.4 3 0 "requires factions { moors, }"
    , RecruitPool "Arab Cavalry" 1 0.4 3 0 "requires factions { egypt, }"
    , RecruitPool "Turkomans" 1 0.4 3 0 "requires factions { turks, }"
    , RecruitPool "Mongol Horse Archers" 1 0.4 3 0 "requires factions { mongols, }"
    , RecruitPool "Turkish Horse Archers" 1 0.4 3 0 "requires factions { timurids, }"
    , RecruitPool "Theigns" 1 0.4 3 0 "requires factions { Saxons, }"
    , RecruitPool "Peasants" 0 0.000001 0.999 0 "requires factions { england, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, }"
    , RecruitPool "Highland Rabble" 0 0.000001 0.999 0 "requires factions { scotland, }"
    , RecruitPool "Southern Peasants" 0 0.000001 0.999 0 "requires factions { sicily, byzantium, }"
    , RecruitPool "EE Peasants" 0 0.000001 0.999 0 "requires factions { poland, russia, hungary, }"
    , RecruitPool "ME Peasants" 0 0.000001 0.999 0 "requires factions { moors, egypt, turks, mongols, timurids, }"
    , RecruitPool "Levy Spearmen" 0 0.000001 0.999 0 "requires factions { england, }"
    , RecruitPool "Highlanders" 0 0.000001 0.999 0 "requires factions { scotland, }"
    , RecruitPool "Sergeant Spearmen" 0 0.000001 0.999 0 "requires factions { france, hre, milan, venice, papal_states, sicily, Normans, }"
    , RecruitPool "Viking Raiders" 0 0.000001 0.999 0 "requires factions { denmark, }"
    , RecruitPool "Javelinmen" 0 0.000001 0.999 0 "requires factions { spain, }"
    , RecruitPool "Lusitanian Javelinmen" 0 0.000001 0.999 0 "requires factions { portugal, }"
    , RecruitPool "Woodsmen" 0 0.000001 0.999 0 "requires factions { poland, russia, }"
    , RecruitPool "Slav Levies" 0 0.000001 0.999 0 "requires factions { hungary, }"
    , RecruitPool "Byzantine Spearmen" 0 0.000001 0.999 0 "requires factions { byzantium, }"
    , RecruitPool "Berber Spearmen" 0 0.000001 0.999 0 "requires factions { moors, }"
    , RecruitPool "Kurdish Javelinmen" 0 0.000001 0.999 0 "requires factions { egypt, }"
    , RecruitPool "Turkish Javelinmen" 0 0.000001 0.999 0 "requires factions { turks, }"
    , RecruitPool "ME Levy Spearmen" 0 0.000001 0.999 0 "requires factions { mongols, }"
    , RecruitPool "Afghan Javelinmen" 0 0.000001 0.999 0 "requires factions { timurids, }"
    , RecruitPool "Peasant Spearmen" 0 0.000001 0.999 0 "requires factions { Saxons, }"
    , RecruitPool "Peasant Archers" 0 0.000001 0.999 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, }"
    , RecruitPool "S Peasant Archers" 0 0.000001 0.999 0 "requires factions { sicily, hungary, byzantium, }"
    , RecruitPool "EE Peasant Archers" 0 0.000001 0.999 0 "requires factions { poland, russia, }"
    , RecruitPool "Sudanese Javelinmen" 0 0.000001 0.999 0 "requires factions { moors, }"
    , RecruitPool "ME Peasant Archers" 0 0.000001 0.999 0 "requires factions { egypt, turks, mongols, timurids, }"
    , RecruitPool "Dismounted Feudal Knights" 1 0.4 3 0 "requires factions { scotland, denmark, }"
    , RecruitPool "Dismounted Noble Knights" 1 0.4 3 0 "requires factions { france, }"
    , RecruitPool "Dismounted Imperial Knights" 1 0.4 3 0 "requires factions { hre, }"
    , RecruitPool "Dismounted Chivalric Knights" 1 0.4 3 0 "requires factions { spain, }"
    , RecruitPool "Dismounted Italian MAA" 1 0.4 3 0 "requires factions { milan, venice, papal_states, }"
    , RecruitPool "Dismounted Norman Knights" 1 0.4 3 0 "requires factions { sicily, }"
    , RecruitPool "Dismounted Boyar Sons" 1 0.4 3 0 "requires factions { russia, }"
    , RecruitPool "Dismounted E Chivalric Knights" 1 0.4 3 0 "requires factions { hungary, }"
    , RecruitPool "Dismounted Arab Cavalry" 1 0.4 3 0 "requires factions { moors, egypt, }"
    , RecruitPool "Dismounted Sipahi Lancers" 1 0.4 3 0 "requires factions { turks, }"
    , RecruitPool "Dismounted Heavy Archers" 1 0.4 3 0 "requires factions { mongols, timurids, }"
    , RecruitPool "Dismounted English Knights" 1 0.5 4 0 "requires factions { england, }"
    , RecruitPool "Dismounted Portuguese Knights" 1 0.5 4 0 "requires factions { portugal, }"
    , RecruitPool "Dismounted Feudal Knights" 1 0.7 6 0 "requires factions { england, france, hre, spain, portugal, milan, venice, papal_states, hungary, }"
    , RecruitPool "Arab Cavalry" 1 0.7 6 0 "requires factions { moors, }"
    , RecruitPool "Mamluk Archers" 1 0.7 6 0 "requires factions { egypt, }"
    , RecruitPool "Sipahis" 1 0.7 6 0 "requires factions { turks, }"
    , RecruitPool "Mongol Light Lancers" 1 0.7 6 0 "requires factions { mongols, }"
    , RecruitPool "Turkomans" 1 0.7 6 0 "requires factions { timurids, }"
    , RecruitPool "Mailed Knights" 1 0.4 3 0 "requires factions { england, scotland, }"
    , RecruitPool "Feudal Knights" 1 0.4 3 0 "requires factions { france, hre, spain, milan, venice, papal_states, }"
    , RecruitPool "Dismounted Huscarls" 1 0.4 3 0 "requires factions { denmark, }"
    , RecruitPool "Norman Knights" 1 0.4 3 0 "requires factions { sicily, }"
    , RecruitPool "Polish Nobles" 1 0.4 3 0 "requires factions { poland, }"
    , RecruitPool "Druzhina" 1 0.4 3 0 "requires factions { russia, }"
    , RecruitPool "Hungarian Nobles" 1 0.4 3 0 "requires factions { hungary, }"
    , RecruitPool "Byzantine Cavalry" 1 0.4 3 0 "requires factions { byzantium, }"
    , RecruitPool "Mailed Knights" 1 0.4 3 0 "requires factions { france, hre, spain, portugal, milan, venice, papal_states, sicily, }"
    , RecruitPool "Feudal Knights" 1 0.4 3 0 "requires factions { hungary, }"
    ]
    ++ zip (repeat Equestrian)
    [ RecruitPool "Lancers" 1 0.5 4 0 "requires factions { france, }"
    , RecruitPool "Gothic Knights" 1 0.5 4 0 "requires factions { hre, }"
    , RecruitPool "Stradiots" 1 0.5 4 0 "requires factions { venice, }"
    , RecruitPool "Polish Guard" 1 0.5 4 0 "requires factions { poland, }"
    , RecruitPool "Tsars Guard" 1 0.5 4 0 "requires factions { russia, }"
    , RecruitPool "Royal Banderium" 1 0.5 4 0 "requires factions { hungary, }"
    , RecruitPool "Kataphractoi" 1 0.5 4 0 "requires factions { byzantium, }"
    , RecruitPool "Christian Guard" 1 0.5 4 0 "requires factions { moors, }"
    , RecruitPool "Royal Mamluks" 1 0.5 4 0 "requires factions { egypt, }"
    , RecruitPool "Quapukulu" 1 0.5 4 0 "requires factions { turks, }"
    , RecruitPool "Khan's Guard" 1 0.5 4 0 "requires factions { mongols, timurids, }"
    , RecruitPool "Reiters" 1 0.5 4 0 "requires factions { hre, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "English Knights" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Chivalric Knights" 1 0.7 6 0 "requires factions { france, denmark, spain, sicily, }"
    , RecruitPool "Imperial Knights" 1 0.7 6 0 "requires factions { hre, }"
    , RecruitPool "Portuguese Knights" 1 0.7 6 0 "requires factions { portugal, }"
    , RecruitPool "Italian MAA" 1 0.7 6 0 "requires factions { milan, venice, papal_states, }"
    , RecruitPool "Polish Knights" 1 0.7 6 0 "requires factions { poland, }"
    , RecruitPool "Dvor Cavalry" 1 0.7 6 0 "requires factions { russia, }"
    , RecruitPool "E Chivalric Knights" 1 0.7 6 0 "requires factions { hungary, }"
    , RecruitPool "Latinkon" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "Granadine Lancers" 1 0.7 6 0 "requires factions { moors, }"
    , RecruitPool "Mamluks" 1 0.7 6 0 "requires factions { egypt, }"
    , RecruitPool "Sipahi Lancers" 1 0.7 6 0 "requires factions { turks, }"
    , RecruitPool "Mongol Heavy Lancers" 1 0.7 6 0 "requires factions { mongols, timurids, }"
    , RecruitPool "Feudal Knights" 1 0.7 6 1 "requires factions { scotland, }"
    , RecruitPool "Druzhina" 1 0.7 6 0 "requires factions { russia, }"
    , RecruitPool "Byzantine Lancers" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "Granadine Jinetes" 1 0.7 6 0 "requires factions { moors, }"
    , RecruitPool "Mamluk Archers" 1 0.7 6 0 "requires factions { egypt, }"
    , RecruitPool "Sipahis" 1 0.7 6 0 "requires factions { turks, }"
    , RecruitPool "Mongol Heavy Archers" 1 0.7 6 0 "requires factions { mongols, timurids, }"
    , RecruitPool "Mailed Knights" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Boyar Sons" 1 0.7 6 0 "requires factions { russia, }"
    , RecruitPool "Byzantine Cavalry" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "Arab Cavalry" 1 0.7 6 0 "requires factions { moors, egypt, }"
    , RecruitPool "Turkomans" 1 0.7 6 0 "requires factions { turks, timurids, }"
    , RecruitPool "Mongol Light Lancers" 1 0.7 6 0 "requires factions { mongols, }"
    , RecruitPool "Hobilars" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Border Horse" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Mounted Sergeants" 1 0.7 6 0 "requires factions { france, milan, venice, papal_states, sicily, }"
    , RecruitPool "Scouts" 1 0.7 6 0 "requires factions { denmark, }"
    , RecruitPool "Jinetes" 1 0.7 6 0 "requires factions { spain, portugal, }"
    , RecruitPool "Polish Shooters" 1 0.7 6 0 "requires factions { poland, }"
    , RecruitPool "Kazaks" 1 0.7 6 0 "requires factions { russia, }"
    , RecruitPool "Magyar Cavalry" 1 0.7 6 0 "requires factions { hungary, }"
    , RecruitPool "Skythikon" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "Desert Cavalry" 1 0.7 6 0 "requires factions { moors, egypt, }"
    , RecruitPool "Turkish Horse Archers" 1 0.7 6 0 "requires factions { turks, timurids, }"
    , RecruitPool "Mongol Horse Archers" 1 0.7 6 0 "requires factions { mongols, }"
    , RecruitPool "Mailed Knights" 1 0.7 6 0 "requires factions { Normans, }"
    , RecruitPool "Hussars" 1 0.7 6 0 "requires factions { poland, }"
    , RecruitPool "Cossack Cavalry" 1 0.7 6 0 "requires factions { russia, }"
    , RecruitPool "Elephants" 1 0.4 3 0 "requires factions { timurids, }"
    , RecruitPool "Elephant Artillery" 1 0.4 3 0 "requires factions { timurids, }"
    , RecruitPool "Feudal Knights" 1 0.7 6 0 "requires factions { england, france, hre, denmark, spain, portugal, milan, venice, papal_states, hungary, }"
    , RecruitPool "Norman Knights" 1 0.7 6 0 "requires factions { sicily, }"
    , RecruitPool "Mailed Knights" 1 0.7 6 0 "requires factions { england, france, hre, spain, portugal, milan, venice, papal_states, sicily, }"
    , RecruitPool "Huscarls" 1 0.7 6 0 "requires factions { denmark, }"
    , RecruitPool "Polish Nobles" 1 0.7 6 0 "requires factions { poland, }"
    , RecruitPool "Hungarian Nobles" 1 0.7 6 0 "requires factions { hungary, }"
    , RecruitPool "Mounted Sergeants" 1 0.7 6 0 "requires factions { hre, }"
    , RecruitPool "Mailed Knights" 1 0.7 6 0 "requires factions { Normans, }"
    ]
    ++ zip (repeat Barracks)
    [ RecruitPool "Musketeers" 1 0.5 4 0 "requires factions { spain, portugal, milan, venice, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Janissary Musketeers" 1 0.5 4 0 "requires factions { turks, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Arquebusiers" 1 0.7 6 0 "requires factions { england, france, hre, denmark, spain, milan, venice, papal_states, sicily, poland, russia, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Portuguese Arquebusiers" 1 0.7 6 0 "requires factions { portugal, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Sudanese Gunners" 1 0.7 6 0 "requires factions { moors, egypt, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Janissary Archers" 1 0.7 6 0 "requires factions { turks, }"
    , RecruitPool "Pikemen" 1 0.7 6 0 "requires factions { france, }"
    , RecruitPool "Heavy Bill Militia" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Pike Militia" 1 0.7 6 0 "requires factions { france, hre, spain, portugal, milan, venice, papal_states, sicily, }"
    , RecruitPool "Hand Gunners" 1 0.7 6 0 "requires factions { denmark, poland, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Berdiche Axemen" 1 0.7 6 0 "requires factions { russia, }"
    , RecruitPool "Arquebusiers" 1 0.7 6 0 "requires factions { hungary, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Varangian Guard" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "ME Hand Gunners" 1 0.7 6 0 "requires factions { moors, turks, timurids, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Tabardariyya" 1 0.7 6 0 "requires factions { egypt, }"
    , RecruitPool "Hand Gunners" 1 0.7 6 0 "requires factions { hre, spain, portugal, milan, venice, papal_states, sicily, }  and event_counter gunpowder_discovered 1"
    , RecruitPool "Bill Militia" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Heavy Pike Militia" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Partisan Militia" 1 0.7 6 0 "requires factions { france, }"
    , RecruitPool "Halberd Militia" 1 0.7 6 0 "requires factions { hre, papal_states, sicily, poland, hungary, }"
    , RecruitPool "Swordstaff Militia" 1 0.7 6 0 "requires factions { denmark, }"
    , RecruitPool "Swordsmen Militia" 1 0.7 6 0 "requires factions { spain, portugal, }"
    , RecruitPool "Italian Cavalry Militia" 1 0.7 6 0 "requires factions { milan, venice, }"
    , RecruitPool "EE Cavalry Militia" 1 0.7 6 0 "requires factions { russia, }"
    , RecruitPool "Byzantine Infantry" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "Urban Militia" 1 0.7 6 0 "requires factions { moors, }"
    , RecruitPool "ME Halberd Militia" 1 0.7 6 0 "requires factions { egypt, turks, timurids, }"
    , RecruitPool "Archer Militia" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Scots Pike Militia" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Crossbow Militia" 1 0.7 6 0 "requires factions { france, hre, denmark, spain, portugal, }"
    , RecruitPool "Genoese Crossbow Militia" 1 0.7 6 0 "requires factions { milan, }"
    , RecruitPool "Pavise Crossbow Militia" 1 0.7 6 0 "requires factions { venice, papal_states, sicily, hungary, }"
    , RecruitPool "EE Crossbow Militia" 1 0.7 6 0 "requires factions { poland, russia, }"
    , RecruitPool "S Archer Militia" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "ME Crossbow Militia" 1 0.7 6 0 "requires factions { moors, }"
    , RecruitPool "Saracen Militia" 1 0.7 6 0 "requires factions { egypt, turks, }"
    , RecruitPool "Sabadar Militia" 1 0.7 6 0 "requires factions { timurids, }"
    , RecruitPool "Spear Militia" 1 0.7 6 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, Normans, }"
    , RecruitPool "Italian Spear Militia" 1 0.7 6 0 "requires factions { milan, venice, papal_states, sicily, }"
    , RecruitPool "EE Spear Militia" 1 0.7 6 0 "requires factions { poland, russia, hungary, }"
    , RecruitPool "SE Spear Militia" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "ME Spear Militia" 1 0.7 6 0 "requires factions { moors, egypt, turks, timurids, }"
    , RecruitPool "Town Militia" 1 0.7 6 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, Normans, }"
    , RecruitPool "Italian Militia" 1 0.7 6 0 "requires factions { milan, venice, papal_states, sicily, }"
    , RecruitPool "EE Town Militia" 1 0.7 6 0 "requires factions { poland, hungary, }"
    , RecruitPool "EE Archer Militia" 1 0.7 6 0 "requires factions { russia, }"
    , RecruitPool "SE Town Militia" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "ME Town Militia" 1 0.7 6 0 "requires factions { moors, turks, timurids, }"
    , RecruitPool "ME Archer Militia" 1 0.7 6 0 "requires factions { egypt, }"
    , RecruitPool "Peasant Spearmen" 1 0.7 6 0 "requires factions { Saxons, }"
    , RecruitPool "ME Spear Militia" 1 0.7 6 1 "requires factions { mongols, }"
    , RecruitPool "Spear Militia" 1 0.7 6 0 "requires factions { Normans, }"
    , RecruitPool "ME Town Militia" 1 0.7 6 0 "requires factions { mongols, }"
    , RecruitPool "Town Militia" 1 0.7 6 0 "requires factions { Normans, }"
    , RecruitPool "Peasant Spearmen" 1 0.7 6 0 "requires factions { Saxons, }"
    ]
    ++ zip (repeat CastleBarracks)
    [ RecruitPool "Noble Swordsmen" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Pikemen" 1 0.7 6 0 "requires factions { france, }"
    , RecruitPool "Dismounted Broken Lances" 1 0.7 6 0 "requires factions { sicily, }"
    , RecruitPool "Dismounted Christian Guard" 1 0.7 6 0 "requires factions { moors, }"
    , RecruitPool "Heavy Billmen" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Noble Pikemen" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Obudshaer" 1 0.7 6 0 "requires factions { denmark, }"
    , RecruitPool "Armored Swordsmen" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Highland Pikemen" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Voulgier" 1 0.7 6 0 "requires factions { france, }"
    , RecruitPool "Zweihander" 1 0.7 6 0 "requires factions { hre, }"
    , RecruitPool "Norse Axemen" 1 0.7 6 0 "requires factions { denmark, }"
    , RecruitPool "Sword and Buckler Men" 1 0.7 6 0 "requires factions { spain, sicily, }"
    , RecruitPool "Aventuros" 1 0.7 6 0 "requires factions { portugal, }"
    , RecruitPool "Venetian Heavy Infantry" 1 0.7 6 1 "requires factions { venice, }"
    , RecruitPool "Dismounted Druchima" 1 0.7 6 0 "requires factions { russia, }"
    , RecruitPool "Pavise Spearmen" 1 0.7 6 1 "requires factions { hungary, }"
    , RecruitPool "Dismounted Latinkon" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "Lamtuna Spearmen" 1 0.7 6 1 "requires factions { moors, }"
    , RecruitPool "Naffatun" 1 0.7 6 1 "requires factions { egypt, turks, }"
    , RecruitPool "Naffatun" 1 0.7 6 0 "requires factions { mongols, timurids, }"
    , RecruitPool "Dismounted Huscarls" 1 0.7 6 0 "requires factions { denmark, }"
    , RecruitPool "Billmen" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Highland Nobles" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Armored Sergeants" 1 0.7 6 1 "requires factions { france, hre, milan, venice, papal_states, sicily, }"
    , RecruitPool "Norse Swordsmen" 1 0.7 6 0 "requires factions { denmark, }"
    , RecruitPool "Almughavars" 1 0.7 6 1 "requires factions { spain, portugal, }"
    , RecruitPool "EE Spearmen" 1 0.7 6 1 "requires factions { poland, russia, }"
    , RecruitPool "Croat Axemen" 1 0.7 6 0 "requires factions { hungary, }"
    , RecruitPool "Dismounted Byzantine Lancers" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "Nubian Spearmen" 1 0.7 6 1 "requires factions { moors, egypt, }"
    , RecruitPool "Azabs" 1 0.7 6 1 "requires factions { turks, }"
    , RecruitPool "Mongol Infantry" 1 0.7 6 1 "requires factions { mongols, }"
    , RecruitPool "Afghan Javelinmen" 1 0.7 6 1 "requires factions { timurids, }"
    , RecruitPool "Armored Sergeants" 1 0.7 6 0 "requires factions { Normans, }"
    , RecruitPool "Levy Spearmen" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Highlanders" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Sergeant Spearmen" 1 0.7 6 1 "requires factions { france, hre, milan, venice, papal_states, sicily, }"
    , RecruitPool "Viking Raiders" 1 0.7 6 0 "requires factions { denmark, }"
    , RecruitPool "Javelinmen" 1 0.7 6 1 "requires factions { spain, }"
    , RecruitPool "Lusitanian Javelinmen" 1 0.7 6 1 "requires factions { portugal, }"
    , RecruitPool "Woodsmen" 1 0.7 6 0 "requires factions { poland, russia, }"
    , RecruitPool "Slav Levies" 1 0.7 6 0 "requires factions { hungary, }"
    , RecruitPool "Byzantine Spearmen" 1 0.7 6 3 "requires factions { byzantium, }"
    , RecruitPool "Berber Spearmen" 1 0.7 6 1 "requires factions { moors, }"
    , RecruitPool "Kurdish Javelinmen" 1 0.7 6 1 "requires factions { egypt, }"
    , RecruitPool "Turkish Javelinmen" 1 0.7 6 1 "requires factions { turks, }"
    , RecruitPool "ME Levy Spearmen" 1 0.7 6 1 "requires factions { mongols, }"
    , RecruitPool "Sergeant Spearmen" 1 0.7 6 0 "requires factions { Normans, }"
    , RecruitPool "Peasant Spearmen" 1 0.7 6 0 "requires factions { Saxons, }"
    , RecruitPool "Peasants" 1 0.7 6 0 "requires factions { england, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, }"
    , RecruitPool "Highland Rabble" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Southern Peasants" 1 0.7 6 0 "requires factions { sicily, byzantium, }"
    , RecruitPool "EE Peasants" 1 0.7 6 0 "requires factions { poland, russia, hungary, }"
    , RecruitPool "ME Peasants" 1 0.7 6 0 "requires factions { moors, egypt, turks, mongols, timurids, }"
    ]
    ++ zip (repeat ProfessionalMilitary)
    [ RecruitPool "Gendarmes" 1 0.7 6 0 "requires factions { france, spain, }"
    , RecruitPool "Demi Lancers" 1 0.7 6 1 "requires factions { england, }"
    , RecruitPool "French Mounted Archers" 1 0.7 6 0 "requires factions { france, }"
    , RecruitPool "Tercio Pikemen" 1 0.7 6 0 "requires factions { spain, }"
    ]
    ++ zip (repeat Missiles)
    [ RecruitPool "Dismounted Longbowmen" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Aventurier" 1 0.7 6 0 "requires factions { france, }"
    , RecruitPool "Mounted Crossbowmen" 1 0.7 6 0 "requires factions { hre, denmark, spain, portugal, milan, venice, papal_states, sicily, }"
    , RecruitPool "Granadine CB Cav" 1 0.7 6 0 "requires factions { moors, }"
    , RecruitPool "Yeoman Archers" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Noble Highland Archers" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Crossbowmen" 1 0.7 6 0 "requires factions { france, denmark, hungary, }"
    , RecruitPool "Pavise Crossbowmen" 1 0.7 6 0 "requires factions { hre, spain, portugal, papal_states, sicily, }"
    , RecruitPool "Genoese Crossbowmen" 1 0.7 6 0 "requires factions { milan, }"
    , RecruitPool "Venetian Archers" 1 0.7 6 0 "requires factions { venice, }"
    , RecruitPool "Lithuanian Cavalry" 1 0.7 6 0 "requires factions { poland, }"
    , RecruitPool "Byzantine Guard Archers" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "ME Peasant Crossbowmen" 1 0.7 6 0 "requires factions { moors, }"
    , RecruitPool "Nubian Archers" 1 0.7 6 1 "requires factions { egypt, }"
    , RecruitPool "Ottoman Infantry" 1 0.7 6 1 "requires factions { turks, }"
    , RecruitPool "Longbowmen" 1 0.7 6 0 "requires factions { england, }"
    , RecruitPool "Highland Archers" 1 0.7 6 0 "requires factions { scotland, }"
    , RecruitPool "Peasant Crossbowmen" 1 0.7 6 0 "requires factions { france, hre, spain, portugal, milan, venice, papal_states, }"
    , RecruitPool "Norse Archers" 1 0.7 6 0 "requires factions { denmark, }"
    , RecruitPool "Sicilian Muslim Archers" 1 0.7 6 0 "requires factions { sicily, }"
    , RecruitPool "Lithuanian Archers" 1 0.7 6 0 "requires factions { poland, }"
    , RecruitPool "Bosnian Archers" 1 0.7 6 0 "requires factions { hungary, }"
    , RecruitPool "Trebizond Archers" 1 0.7 6 0 "requires factions { byzantium, }"
    , RecruitPool "Desert Archers" 1 0.7 6 0 "requires factions { moors, egypt, }"
    , RecruitPool "Turkish Archers" 1 0.7 6 0 "requires factions { turks, timurids, }"
    , RecruitPool "Mongol Foot Archers" 1 0.7 6 0 "requires factions { mongols, }"
    , RecruitPool "Peasant Archers" 1 0.7 6 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, }"
    , RecruitPool "S Peasant Archers" 1 0.7 6 0 "requires factions { sicily, hungary, byzantium, }"
    , RecruitPool "EE Peasant Archers" 1 0.7 6 0 "requires factions { poland, }"
    , RecruitPool "EE Peasant Archers" 1 0.7 6 1 "requires factions { russia, }"
    , RecruitPool "Sudanese Javelinmen" 1 0.7 6 0 "requires factions { moors, }"
    , RecruitPool "ME Peasant Archers" 1 0.7 6 0 "requires factions { egypt, turks, mongols, timurids, }"
    ]
    ++ zip (repeat Siege) siegeRecruits
    ++ zip (repeat CastleSiege) siegeRecruits
    ++ zip (repeat Cannon) cannonRecruits
    ++ zip (repeat CastleCannon) cannonRecruits
    ++ zip (repeat UrbanEquestrian)
    [ RecruitPool "Arab Cavalry" 1 0.5 4 0 "requires factions { moors, egypt, }"
    , RecruitPool "Turkomans" 1 0.5 4 0 "requires factions { turks, timurids, }"
    , RecruitPool "Mongol Light Lancers" 1 0.5 4 0 "requires factions { mongols, }"
    , RecruitPool "Granadine Jinetes" 1 0.4 3 0 "requires factions { moors, }"
    , RecruitPool "Mamluk Archers" 1 0.4 3 0 "requires factions { egypt, }"
    , RecruitPool "Sipahis" 1 0.4 3 0 "requires factions { turks, }"
    , RecruitPool "Mongol Heavy Archers" 1 0.4 3 0 "requires factions { mongols, timurids, }"
    ]
    ++ zip (repeat Port) portRecruits
    ++ zip (repeat CastlePort) portRecruits
    ++ zip (repeat Tavern)
    [ RecruitPool "Transilvanian Peasants" 1 0.5 4 0 "requires factions { hungary, }"
    ]
    ++ zip (repeat CityHall)
    [ RecruitPool "Forlorn Hope" 1 0.4 3 0 "requires factions { hre, }"
    , RecruitPool "Carroccio Standard M" 1 0.4 3 0 "requires factions { milan, }"
    , RecruitPool "Carroccio Standard V" 1 0.4 3 0 "requires factions { venice, }"
    , RecruitPool "Famiglia Ducale" 1 0.5 4 0 "requires factions { milan, }"
    , RecruitPool "Janissary Heavy Inf" 1 0.5 4 0 "requires factions { turks, }"
    ]
    ++ zip (repeat Bullring)
    [ RecruitPool "Jinetes" 1 0.5 4 0 "requires factions { spain, portugal, }"
    ]
    ++ zip (repeat Caravan)
    [ RecruitPool "Tuareg Camel Spearmens" 1 0.5 4 0 "requires factions { moors, }"
    , RecruitPool "Camel Gunners" 1 0.4 3 0 "requires factions { moors, }  and event_counter gunpowder_discovered 1"
    ]
    ++ zip (repeat AssassinsGuild)
    [ RecruitPool "Battlefield Assassins" 1 0.4 3 1 "requires factions { hungary, }"
    ]
    ++ zip (repeat AssassinsMuslimGuild)
    [ RecruitPool "Hashishim" 1 0.4 3 2 "requires factions { moors, egypt, turks, }"
    ]
-- Note: there is some ugly, hacky (though harmless) code in the vanilla M2TW; we can't just copy
-- the guild HQ's units over here b/c that would leave out some other units, like Sowrdsmen Militia
    ++ zip (repeat MasonsGuild)
    [ RecruitPool "Berdiche Axemen" 1 0.5 4 0 "requires factions { russia, }"
    , RecruitPool "Bill Militia" 1 0.5 4 0 "requires factions { england, }"
    , RecruitPool "Byzantine Infantry" 1 0.5 4 0 "requires factions { byzantium, }"
    , RecruitPool "EE Spear Militia" 1 0.5 4 0 "requires factions { russia, }"
    , RecruitPool "Halberd Militia" 1 0.5 4 0 "requires factions { hre, papal_states, sicily, poland, hungary, }"
    , RecruitPool "Heavy Bill Militia" 1 0.5 4 0 "requires factions { england, }"
    , RecruitPool "Heavy Pike Militia" 1 0.5 4 0 "requires factions { scotland, }"
    , RecruitPool "Italian Spear Militia" 1 0.5 4 0 "requires factions { milan, venice, }"
    , RecruitPool "ME Halberd Militia" 1 0.5 4 0 "requires factions { egypt, turks, timurids, }"
    , RecruitPool "ME Spear Militia" 1 0.5 4 0 "requires factions { mongols, }"
    , RecruitPool "Partisan Militia" 1 0.5 4 0 "requires factions { france, }"
    , RecruitPool "Pike Militia" 1 0.5 4 0 "requires factions { france, hre, spain, portugal, milan, venice, papal_states, sicily, }"
    , RecruitPool "Swordsmen Militia" 1 0.5 4 0 "requires factions { spain, portugal, }"
    , RecruitPool "Swordstaff Militia" 1 0.5 4 0 "requires factions { denmark, }"
    , RecruitPool "Urban Militia" 1 0.5 4 0 "requires factions { moors, }"
    ]
    ++ zip (repeat MerchantsGuild)
-- Give +1 XP for England's Merchant Cavalry Militia as well, just like everyone else. (Bug in M2TW
-- 1.3).
    [ RecruitPool "Merchant Cavalry Militia" 1 0.5 4 1 "requires factions { england, scotland, france, hre, denmark, spain, portugal, poland, hungary, }"
    , RecruitPool "Italian Cavalry Militia" 1 0.5 4 1 "requires factions { milan, venice, papal_states, sicily, }"
    , RecruitPool "EE Cavalry Militia" 1 0.5 4 1 "requires factions { russia, }"
    , RecruitPool "Greek Militia Cavalry" 1 0.5 4 1 "requires factions { byzantium, }"
    , RecruitPool "Arab Cavalry" 1 0.5 4 1 "requires factions { moors, egypt, }"
    , RecruitPool "Sipahis" 1 0.5 4 1 "requires factions { turks, }"
    ]
    ++ zip (repeat TemplarsChapterHouse)
    [ RecruitPool"Knights Templar" 1 0.7 6 2 "requires factions { england, scotland, france, denmark, milan, venice, papal_states, sicily, poland, hungary, }"
    ]
    ++ zip (repeat StJohnsChapterHouse)
    [ RecruitPool"Knights Hospitaller" 1 0.7 6 2 "requires factions { england, scotland, france, denmark, milan, venice, papal_states, sicily, poland, hungary, }"
    ]
    ++ zip (repeat TeutonicKnightsChapterHouse)
    [ RecruitPool"Teutonic Knights" 1 0.7 6 2 "requires factions { hre, }"
    ]
    ++ zip (repeat KnightsOfSantiagoChapterHouse)
    [ RecruitPool"Knights of Santiago" 1 0.7 6 2 "requires factions { spain, portugal, }"
    ]
    ++ zip (repeat WoodsmensGuild)
    [ RecruitPool"Sherwood Archers" 1 0.4 3 0 "requires factions { england, }"
    ]
    where
        siegeRecruits =
            [ RecruitPool "NE Trebuchet" 1 0.4 3 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, }"
            , RecruitPool "EE Trebuchet" 1 0.4 3 0 "requires factions { poland, russia, hungary, }"
            , RecruitPool "GR Trebuchet" 1 0.4 3 0 "requires factions { byzantium, }"
            , RecruitPool "ME Trebuchet" 1 0.4 3 0 "requires factions { moors, egypt, turks, }"
            , RecruitPool "AS Trebuchet" 1 0.4 3 0 "requires factions { mongols, timurids, }"
            , RecruitPool "NE Catapult" 1 0.4 3 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, Normans, }"
            , RecruitPool "EE Catapult" 1 0.4 3 0 "requires factions { poland, russia, hungary, }"
            , RecruitPool "GR Catapult" 1 0.4 3 0 "requires factions { byzantium, }"
            , RecruitPool "ME Catapult" 1 0.4 3 0 "requires factions { moors, egypt, turks, }"
            , RecruitPool "AS Catapult" 1 0.4 3 0 "requires factions { mongols, timurids, }"
            , RecruitPool "NE Ballista" 1 0.4 3 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, Normans, }"
            , RecruitPool "EE Ballista" 1 0.4 3 0 "requires factions { poland, russia, hungary, }"
            , RecruitPool "GR Ballista" 1 0.4 3 0 "requires factions { byzantium, }"
            , RecruitPool "ME Ballista" 1 0.4 3 0 "requires factions { moors, egypt, turks, }"
            , RecruitPool "AS Ballista" 1 0.4 3 0 "requires factions { mongols, timurids, }"
            ]
        cannonRecruits =
            [ RecruitPool "NE Basilisk" 1 0.4 3 0 "requires factions { france, hre, spain, portugal, }"
            , RecruitPool "NE Monster Ribault" 1 0.4 3 0 "requires factions { milan, venice, }"
            , RecruitPool "EE Basilisk" 1 0.4 3 0 "requires factions { russia, hungary, }"
            , RecruitPool "ME Monster Bombard" 1 0.4 3 0 "requires factions { turks, }"
            , RecruitPool "NE Culverin" 1 0.4 3 0 "requires factions { england, scotland, france, spain, portugal, milan, venice, papal_states, }"
            , RecruitPool "NE Cannon" 1 0.4 3 0 "requires factions { hre, sicily, }"
            , RecruitPool "NE Serpentine" 1 0.4 3 0 "requires factions { denmark, }"
            , RecruitPool "EE Serpentine" 1 0.4 3 0 "requires factions { poland, hungary, }"
            , RecruitPool "EE Cannon" 1 0.4 3 0 "requires factions { russia, }"
            , RecruitPool "ME Cannon" 1 0.4 3 0 "requires factions { moors, egypt, turks, }"
            , RecruitPool "AS Cannon" 1 0.4 3 0 "requires factions { timurids, }"
            , RecruitPool "NE Serpentine" 1 0.4 3 0 "requires factions { france, hre, }"
            , RecruitPool "NE Cannon" 1 0.4 3 0 "requires factions { denmark, }"
            , RecruitPool "EE Cannon" 1 0.4 3 0 "requires factions { poland, }"
            , RecruitPool "NE Mortar" 1 0.4 3 0 "requires factions { england, scotland, venice, papal_states, sicily, }"
            , RecruitPool "NE Grand Bombard" 1 0.4 3 0 "requires factions { france, hre, spain, portugal, milan, }"
            , RecruitPool "NE Ribault" 1 0.4 3 0 "requires factions { denmark, }"
            , RecruitPool "EE Ribault" 1 0.4 3 0 "requires factions { poland, hungary, }"
            , RecruitPool "EE Grand Bombard" 1 0.4 3 0 "requires factions { russia, }"
            , RecruitPool "ME Grand Bombard" 1 0.4 3 0 "requires factions { moors, egypt, turks, }"
            , RecruitPool "AS Grand Bombard" 1 0.4 3 0 "requires factions { timurids, }"
            , RecruitPool "NE Ribault" 1 0.4 3 0 "requires factions { england, scotland, spain, portugal, milan, venice, papal_states, sicily, }"
            , RecruitPool "NE Bombard" 1 0.4 3 0 "requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, }"
            , RecruitPool "EE Bombard" 1 0.4 3 0 "requires factions { poland, russia, hungary, }"
            , RecruitPool "GR Bombard" 1 0.4 3 0 "requires factions { byzantium, }"
            , RecruitPool "ME Bombard" 1 0.4 3 0 "requires factions { moors, egypt, turks, }"
            , RecruitPool "AS Rocket Launcher" 1 0.4 3 0 "requires factions { mongols, }"
            , RecruitPool "AS Bombard" 1 0.4 3 0 "requires factions { timurids, }"
            ]
        portRecruits =
            [ RecruitPool "cog" 1 1 3 0 "requires factions { hre, france, scotland, england, }"
            , RecruitPool "longboat" 1 1 3 0 "requires factions { denmark, }"
            , RecruitPool "holk" 1 1 3 0 "requires factions { hre, scotland, france, england, }"
            , RecruitPool "dragon boat" 1 1 3 0 "requires factions { denmark, }"
            , RecruitPool "gun holk" 1 1 3 0 "requires factions { northern_european, }"
            , RecruitPool "carrack" 1 1 3 0 "requires factions { northern_european, } and event_counter world_is_round 1"
            , RecruitPool "dhow" 1 1 3 0 "requires factions { middle_eastern, }"
            , RecruitPool "war galley" 1 1 3 0 "requires factions { egypt, moors, turks, }"
            , RecruitPool "lanternas" 1 1 3 0 "requires factions { egypt, moors, turks, }"
            , RecruitPool "baghlah" 1 1 3 0 "requires factions { egypt, moors, turks, } and event_counter world_is_round 1"
            , RecruitPool "cog" 1 1 3 0 "requires factions { hungary, poland, }"
            , RecruitPool "ladya" 1 1 3 0 "requires factions { russia, }"
            , RecruitPool "war galley" 1 1 3 0 "requires factions { hungary, }"
            , RecruitPool "holk" 1 1 3 0 "requires factions { russia, poland, }"
            , RecruitPool "gun holk" 1 1 3 0 "requires factions { russia, poland, }"
            , RecruitPool "lanternas" 1 1 3 0 "requires factions { hungary, }"
            , RecruitPool "carrack" 1 1 3 0 "requires factions { eastern_european, } and event_counter world_is_round 1"
            , RecruitPool "dromon" 1 1 3 0 "requires factions { greek, }"
            , RecruitPool "fire ship" 1 1 3 0 "requires factions { greek, }"
            , RecruitPool "lanternas" 1 1 3 0 "requires factions { greek, }"
            , RecruitPool "carrack" 1 1 3 0 "requires factions { greek, } and event_counter world_is_round 1"
            , RecruitPool "galley" 1 1 3 0 "requires factions { sicily, papal_states, milan, venice, }"
            , RecruitPool "cog" 1 1 3 0 "requires factions { portugal, spain, }"
            , RecruitPool "war galley" 1 1 3 0 "requires factions { southern_european, }"
            , RecruitPool "lanternas" 1 1 3 0 "requires factions { portugal, spain, papal_states, milan, sicily, }"
            , RecruitPool "caravel" 1 1 3 0 "requires factions { portugal, spain, } and event_counter world_is_round 1"
            , RecruitPool "galleass" 1 1 3 0 "requires factions { venice, }"
            , RecruitPool "carrack" 1 1 3 0 "requires factions { papal_states, sicily, milan, venice, } and event_counter world_is_round 1"
            , RecruitPool "grande carrack" 1 1 3 0 "requires factions { portugal, spain, } and event_counter world_is_round 1"
            ]
