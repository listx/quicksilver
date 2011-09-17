{-# LANGUAGE RecordWildCards #-}

module Data.Recruit.RTW where

import Util

data RecruitPool = RecruitPool
    { unitName :: String
    , experience :: Int
    , req :: String
    }

instance Show RecruitPool where
    show RecruitPool{..} = "                recruit "
        ++ dquote unitName ++ " "
        ++ show experience ++ " "
        ++ req

data Building
    = Core
    | Barracks
    | Equestrian
    | Missiles
    | Smith
    | Port
    | Amphitheatres
    | TempleOfBattle
    | TempleOfBattleforge
    | TempleOfFertility
    | TempleOfHealing
    | TempleOfJustice
    | TempleOfLaw
    | TempleOfLeadership
    | TempleOfLove
    | TempleOfNaval
    | TempleOfVictory
    | TempleOfViolence
    | TempleOfViking
    deriving (Eq)

instance Show Building where
    show b = case b of
        Core -> "core_building"
        Barracks -> "barracks"
        Equestrian -> "equestrian"
        Missiles -> "missiles"
        Smith -> "smith"
        Port -> "port"
        Amphitheatres -> "amphitheatres"
        TempleOfBattle -> "temple_of_battle"
        TempleOfBattleforge -> "temple_of_battleforge"
        TempleOfFertility -> "temple_of_fertility"
        TempleOfHealing -> "temple_of_healing"
        TempleOfJustice -> "temple_of_justice"
        TempleOfLaw -> "temple_of_law"
        TempleOfLeadership -> "temple_of_leadership"
        TempleOfLove -> "temple_of_love"
        TempleOfNaval -> "temple_of_naval"
        TempleOfVictory -> "temple_of_victory"
        TempleOfViolence -> "temple_of_violence"
        _ -> "temple_of_viking" --TempleOfViking

town, largeTown, city, largeCity, hugeCity :: Int
town = 1 -- town
largeTown = 2 -- large_town
city = 3 -- city ("Minor City" in-game)
largeCity = 5 -- large_city
hugeCity = 7 -- huge_city

modRecruit :: Int -> RecruitPool -> String
modRecruit e RecruitPool{..} = show RecruitPool
    { unitName = unitName
    , experience = experience + e
    , req = req
    }

getRecruits :: Building -> Int -> String
getRecruits btype rmod = concatMap (\s -> modRecruit rmod s ++ "\r\n") (map snd . filter (\(a, _) -> a == btype) $ _RECRUIT_DATA)

_RECRUIT_DATA :: [(Building, RecruitPool)]
_RECRUIT_DATA =
    -- NOTE: move agent definitions somewhere else!
    zip (repeat Core)
    [ RecruitPool "carthaginian peasant" 0 "requires factions { spain, }"
    , RecruitPool "barb peasant briton" 0 "requires factions { britons, }"
    , RecruitPool "barb peasant dacian" 0 "requires factions { dacia, }"
    , RecruitPool "barb peasant gaul" 0 "requires factions { gauls, }"
    , RecruitPool "barb peasant german" 0 "requires factions { germans, }"
    , RecruitPool "barb peasant scythian" 0 "requires factions { scythia, }"
                -- agent diplomat  0  requires factions { barbarian, }
    , RecruitPool "carthaginian peasant" 0 "requires factions { carthaginian, }"
                -- agent diplomat  0  requires factions { carthaginian, }
    , RecruitPool "east peasant" 0 "requires factions { eastern, }"
                -- agent diplomat  0  requires factions { eastern, }
    , RecruitPool "egyptian peasant" 0 "requires factions { egyptian, }"
                -- agent diplomat  0  requires factions { egyptian, }
    , RecruitPool "greek peasant" 0 "requires factions { greek, }"
                -- agent diplomat  0  requires factions { greek, }
    , RecruitPool "roman peasant" 0 "requires factions { roman, }"
                -- agent diplomat  0  requires factions { roman, }
    , RecruitPool "roman praetorian cohort i" 0 "requires factions { roman, }  and marian_reforms"
    ]
    ++ zip (repeat Barracks)
    [ RecruitPool "carthaginian city militia" 0 "requires factions { spain, }"
    , RecruitPool "carthaginian infantry" 0 "requires factions { spain, }"
    , RecruitPool "spanish scutarii" 0 "requires factions { spain, }"
    , RecruitPool "barb infantry briton" 0 "requires factions { britons, }"
    , RecruitPool "barb infantry dacian" 0 "requires factions { dacia, }"
    , RecruitPool "barb infantry gaul" 0 "requires factions { gauls, }"
    , RecruitPool "warband spear german" 0 "requires factions { germans, }"
    , RecruitPool "barb horse archers scythian" 0 "requires factions { scythia, }"
    , RecruitPool "warband sword briton" 0 "requires factions { britons, }"
    , RecruitPool "warband sword gaul" 0 "requires factions { gauls, }"
    , RecruitPool "warband axe german" 0 "requires factions { germans, }"
    , RecruitPool "warband falx dacian" 0 "requires factions { dacia, }"
    , RecruitPool "barb warguard gaul" 0 "requires factions { gauls, }"
    , RecruitPool "barb warguard briton" 0 "requires factions { britons, }"
    , RecruitPool "barb warguard dacian" 0 "requires factions { dacia, }"
    , RecruitPool "warband dhaxe german" 0 "requires factions { germans, }"
    , RecruitPool "warband axe scythian" 1 "requires factions { scythia, }"
    , RecruitPool "carthaginian city militia" 0 "requires factions { carthage, }"
    , RecruitPool "numidian javelinmen" 0 "requires factions { numidia, }"
    , RecruitPool "carthaginian infantry" 0 "requires factions { carthage, }"
    , RecruitPool "numidian desert warriors" 1 "requires factions { numidia, }"
    , RecruitPool "carthaginian medium infantry" 0 "requires factions { carthage, }"
    , RecruitPool "carthaginian heavy infantry" 0 "requires factions { carthage, }"
    , RecruitPool "numidian legionaries" 0 "requires factions { numidia, }"
    , RecruitPool "east hillmen" 0 "requires factions { eastern, }"
    , RecruitPool "east infantry" 0 "requires factions { eastern, }"
    , RecruitPool "east heavy infantry" 0 "requires factions { armenia, }"
    , RecruitPool "east hoplite" 0 "requires factions { pontus, }"
    , RecruitPool "east hoplite brazen shield" 0 "requires factions { pontus, }"
    , RecruitPool "east legionary" 0 "requires factions { armenia, }"
    , RecruitPool "egyptian nubian spearmen" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian nile infantry" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian infantry" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian elite guards" 0 "requires factions { egyptian, }"
    , RecruitPool "greek hoplite militia" 0 "requires factions { greek, }"
    , RecruitPool "greek levy pikemen" 0 "requires factions { seleucid, macedon, }"
    , RecruitPool "warband falx thracian" 0 "requires factions { thrace, }"
    , RecruitPool "greek hoplite" 1 "requires factions { greek_cities, }"
    , RecruitPool "greek pikemen" 0 "requires factions { thrace, seleucid, macedon, }"
    , RecruitPool "greek hoplite elite" 0 "requires factions { greek_cities, }"
    , RecruitPool "greek royal pikemen" 0 "requires factions { macedon, }"
    , RecruitPool "greek silver shield pikemen" 0 "requires factions { seleucid, }"
    , RecruitPool "greek hoplite spartan" 0 "requires factions { greek_cities, }  and hidden_resource sparta"
    , RecruitPool "greek bastarnae" 0 "requires factions { thrace, }"
    , RecruitPool "greek argyraspid" 0 "requires factions { seleucid, }"
    , RecruitPool "roman city militia" 0 "requires factions { roman, }"
    , RecruitPool "roman hastati" 0 "requires factions { roman, }  and not marian_reforms"
    , RecruitPool "roman infantry auxillia" 0 "requires factions { roman, }  and marian_reforms"
    , RecruitPool "roman princeps" 0 "requires factions { roman, }  and not marian_reforms"
    , RecruitPool "roman legionary cohort i" 0 "requires factions { roman, }  and marian_reforms"
    , RecruitPool "roman legionary first cohort i" 0 "requires factions { roman, }  and hidden_resource rome and marian_reforms"
    , RecruitPool "roman triarii" 1 "requires factions { roman, }  and not marian_reforms"
    , RecruitPool "roman legionary cohort ii" 0 "requires factions { roman, }  and marian_reforms"
    , RecruitPool "roman legionary first cohort ii" 0 "requires factions { roman, }  and hidden_resource rome and marian_reforms"
    , RecruitPool "roman praetorian cohort urban i" 0 "requires factions { roman, }  and marian_reforms"
    , RecruitPool "roman praetorian cohort i" 0 "requires factions { roman, }  and marian_reforms"
    ]
    ++ zip (repeat Equestrian)
    [ RecruitPool "carthaginian warhounds" 0 "requires factions { spain, }"
    , RecruitPool "carthaginian cavalry" 0 "requires factions { spain, }"
    , RecruitPool "carthaginian medium cavalry" 0 "requires factions { spain, }"
    , RecruitPool "barb cavalry gaul" 0 "requires factions { gauls, }"
    , RecruitPool "barb cavalry german" 0 "requires factions { germans, }"
    , RecruitPool "barb cavalry dacian" 0 "requires factions { dacia, }"
    , RecruitPool "barb cavalry scythian" 0 "requires factions { scythia, }"
    , RecruitPool "barb wardogs briton" 0 "requires factions {  britons, }"
    , RecruitPool "barb wardogs dacian" 0 "requires factions { dacia, }"
    , RecruitPool "barb wardogs gaul" 0 "requires factions { gauls, }"
    , RecruitPool "barb wardogs german" 0 "requires factions { germans, }"
    , RecruitPool "barb wardogs scythian" 0 "requires factions { scythia, }"
    , RecruitPool "barb noble cavalry gaul" 0 "requires factions { gauls, }"
    , RecruitPool "barb noble cavalry german" 0 "requires factions { germans, }"
    , RecruitPool "barb noble cavalry dacian" 0 "requires factions { dacia, }"
    , RecruitPool "barb scythian nobles scythian" 0 "requires factions { scythia, }"
    , RecruitPool "barb noble horse archers scythian" 0 "requires factions { scythia, }"
    , RecruitPool "carthaginian cavalry" 0 "requires factions { carthage, }"
    , RecruitPool "numidian cavalry" 0 "requires factions { numidia, }"
    , RecruitPool "carthaginian medium cavalry" 0 "requires factions { carthaginian, }"
    , RecruitPool "carthaginian elephant forest" 0 "requires factions { carthage, }  and resource elephants"
    , RecruitPool "carthaginian elephant african" 0 "requires factions { carthage, }  and resource elephants"
    , RecruitPool "carthaginian elephant african cataphract" 0 "requires factions { carthage, }  and resource elephants"
    , RecruitPool "numidian camel riders" 0 "requires factions { numidia, }  and resource camels"
    , RecruitPool "carthaginian royal cavalry" 0 "requires factions { carthage, }"
    , RecruitPool "east cavalry" 0 "requires factions { pontus, }"
    , RecruitPool "east horse archer" 0 "requires factions { armenia, parthia, }"
    , RecruitPool "east heavy cavalry" 0 "requires factions { pontus, }"
    , RecruitPool "east persian cavalry" 0 "requires factions { parthia, }"
    , RecruitPool "east cataphract archer" 0 "requires factions { armenia, }"
    , RecruitPool "east cappodocian cavalry" 0 "requires factions { pontus, }"
    , RecruitPool "east heavy cataphract" 0 "requires factions { armenia, parthia, }"
    , RecruitPool "east elephant" 0 "requires factions { parthia, }  and resource elephants"
    , RecruitPool "east camel cataphract" 0 "requires factions { parthia, }  and resource camels"
    , RecruitPool "egyptian cavalry" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian nubian cavalry" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian cleruch" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian bedouin" 0 "requires factions { egyptian, }  and resource camels"
    , RecruitPool "greek light lancer" 0 "requires factions { macedon, }"
    , RecruitPool "greek cavalry" 0 "requires factions { seleucid, thrace, greek_cities, }"
    , RecruitPool "greek medium cavalry" 0 "requires factions { greek, }"
    , RecruitPool "greek elephant small" 0 "requires factions { seleucid, }  and resource elephants"
    , RecruitPool "greek heavy cavalry" 0 "requires factions { macedon, }"
    , RecruitPool "east heavy cataphract" 0 "requires factions { seleucid, }"
    , RecruitPool "greek elephant african" 0 "requires factions { seleucid, }  and resource elephants"
    , RecruitPool "greek royal cavalry" 0 "requires factions { seleucid, macedon, }"
    , RecruitPool "greek elephant cataphract" 0 "requires factions { seleucid, }  and resource elephants"
    , RecruitPool "greek incendiary pigs" 0 "requires factions { greek_cities, }"
    , RecruitPool "roman cavalry auxillia" 0 "requires factions { roman, }"
    , RecruitPool "roman light cavalry" 2 "requires factions { roman, }  and not marian_reforms"
    , RecruitPool "roman medium cavalry" 0 "requires factions { roman, }  and marian_reforms"
    , RecruitPool "roman heavy cavalry" 0 "requires factions { roman, }  and marian_reforms"
    , RecruitPool "roman praetorian cavalry" 0 "requires factions { roman, }  and marian_reforms"
    , RecruitPool "roman wardogs" 0 "requires factions { roman, }"
    , RecruitPool "roman pigs" 0 "requires factions { roman, }"
    ]
    ++ zip (repeat Missiles)
    [ RecruitPool "carthaginian slinger" 0 "requires factions { spain, }"
    , RecruitPool "carthaginian peltast" 0 "requires factions { spain, }"
    , RecruitPool "barb peltast gaul" 0 "requires factions { gauls, }"
    , RecruitPool "barb peltast german" 0 "requires factions { germans, }"
    , RecruitPool "barb slinger briton" 0 "requires factions { britons, }"
    , RecruitPool "barb archer dacian" 0 "requires factions { dacia, }"
    , RecruitPool "barb archer scythian" 0 "requires factions { scythia, }"
    , RecruitPool "warband archer german" 0 "requires factions { germans, }"
    , RecruitPool "warband archer dacian" 0 "requires factions { dacia, }"
    , RecruitPool "warband archer scythian" 0 "requires factions { scythia, }"
    , RecruitPool "warband huntsman gaul" 0 "requires factions { gauls, }"
    , RecruitPool "barb ballista dacian" 0 "requires factions { dacia, }"
    , RecruitPool "barb onager dacian" 0 "requires factions { dacia, }"
    , RecruitPool "barb onager scythian" 0 "requires factions { scythia, }"
    , RecruitPool "carthaginian peltast" 0 "requires factions { carthage, }"
    , RecruitPool "carthaginian archer" 0 "requires factions { numidia, }"
    , RecruitPool "carthaginian slinger" 0 "requires factions { carthaginian, }"
    , RecruitPool "carthaginian onager" 0 "requires factions { carthaginian, }"
    , RecruitPool "carthaginian heavy onager" 0 "requires factions { carthage, }"
    , RecruitPool "east peltast" 0 "requires factions { armenia, pontus, }"
    , RecruitPool "east slinger" 0 "requires factions { parthia, }"
    , RecruitPool "east archer" 0 "requires factions { eastern, }"
    , RecruitPool "east onager" 0 "requires factions { eastern, }"
    , RecruitPool "egyptian peltast" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian slingers" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian archer" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian archer elite" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian onager" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian heavy onager" 0 "requires factions { egyptian, }"
    , RecruitPool "greek peltast" 0 "requires factions { greek, }"
    , RecruitPool "greek archer" 0 "requires factions { greek, }"
    , RecruitPool "greek ballista" 0 "requires factions { macedon, greek_cities, }"
    , RecruitPool "greek heavy peltast" 0 "requires factions { greek_cities, }"
    , RecruitPool "greek onager" 0 "requires factions { greek, }"
    , RecruitPool "greek heavy onager" 0 "requires factions { macedon, greek_cities, }"
    , RecruitPool "roman velite" 0 "requires factions { roman, }  and not marian_reforms"
    , RecruitPool "roman light infantry auxillia" 0 "requires factions { roman, }  and marian_reforms"
    , RecruitPool "roman archer" 0 "requires factions { roman, }  and not marian_reforms"
    , RecruitPool "roman archer auxillia" 0 "requires factions { roman, }  and marian_reforms"
    , RecruitPool "roman ballista" 0 "requires factions { roman, }"
    , RecruitPool "roman scorpion" 0 "requires factions { roman, }"
    , RecruitPool "roman onager" 0 "requires factions { roman, }"
    , RecruitPool "roman heavy onager" 0 "requires factions { roman, }"
    , RecruitPool "roman repeating ballista" 0 "requires factions { roman, }"
    ]
    ++ zip (repeat Smith)
    [ RecruitPool "barb chariot heavy briton" 0 "requires factions { britons, }"
    , RecruitPool "barb chariot light briton" 0 "requires factions { britons, }"
    , RecruitPool "east scythed chariot" 0 "requires factions { pontus, }"
    , RecruitPool "east chariot archer" 0 "requires factions { pontus, }"
    , RecruitPool "egyptian chariot" 0 "requires factions { egyptian, }"
    , RecruitPool "egyptian chariot archer" 0 "requires factions { egyptian, }"
    , RecruitPool "rebel amazon chariots" 2 "requires factions { slave, }"
    , RecruitPool "greek chariot scythed" 0 "requires factions { seleucid, }"
    ]
    ++ zip (repeat Port)
    [ RecruitPool "naval boats" 0 "requires factions { barbarian, }"
    , RecruitPool "naval large boats" 0 "requires factions { barbarian, }"
    , RecruitPool "naval biremes" 0 "requires factions { carthaginian, }"
    , RecruitPool "naval triremes" 0 "requires factions { carthaginian, }"
    , RecruitPool "naval quinquiremes" 0 "requires factions { carthaginian, }"
    , RecruitPool "naval biremes" 0 "requires factions { eastern, }"
    , RecruitPool "naval triremes" 0 "requires factions { eastern, }"
    , RecruitPool "naval quinquiremes" 0 "requires factions { eastern, }"
    , RecruitPool "naval biremes" 0 "requires factions { egyptian, }"
    , RecruitPool "naval triremes" 0 "requires factions { egyptian, }"
    , RecruitPool "naval quinquiremes" 0 "requires factions { egyptian, }"
    , RecruitPool "naval biremes" 0 "requires factions { greek, }"
    , RecruitPool "naval triremes" 0 "requires factions { greek, }"
    , RecruitPool "naval quinquiremes" 0 "requires factions { greek, }"
    , RecruitPool "naval biremes" 0 "requires factions { roman, }"
    , RecruitPool "naval triremes" 0 "requires factions { roman, }"
    , RecruitPool "naval quinquiremes" 0 "requires factions { roman, }"
    ]
    ++ zip (repeat Amphitheatres)
    [ RecruitPool "roman velite gladiator" 0 "requires factions { romans_brutii, }"
    , RecruitPool "roman samnite gladiator" 0 "requires factions { romans_senate, romans_julii, }"
    , RecruitPool "roman mirmillo gladiator" 0 "requires factions { romans_scipii, }"
    ]
    ++ zip (repeat TempleOfBattle)
    [ RecruitPool "barb naked fanatics dacian" 1 "requires factions { dacia, }"
    ]
    ++ zip (repeat TempleOfBattleforge)
    [ RecruitPool "barb naked fanatics gauls" 1 "requires factions { gauls, }"
    ]
    ++ zip (repeat TempleOfFertility)
    [ RecruitPool "barb screeching women german" 0 "requires factions { germans, }"
    ]
    ++ zip (repeat TempleOfHealing)
    [ RecruitPool "barb druids briton" 0 "requires factions { britons, }"
    ]
    ++ zip (repeat TempleOfJustice)
    [ RecruitPool "barb druids gaul" 0 "requires factions { gauls, }"
    , RecruitPool "barb naked fanatics spain" 0 "requires factions { spain, }"
    , RecruitPool "spanish bull warriors" 0 "requires factions { spain, }"
    , RecruitPool "carthaginian sacred band infantry" 0 "requires factions { carthage, }"
    ]
    ++ zip (repeat TempleOfLaw)
    [ RecruitPool "roman arcani" 0 "requires factions { romans_scipii, }"
    ]
    ++ zip (repeat TempleOfLeadership)
    [ RecruitPool "roman arcani" 0 "requires factions { romans_julii, }"
    ]
    ++ zip (repeat TempleOfLove)
    [ RecruitPool "barb head hunting maidens scythian" 0 "requires factions { scythia, }"
    , RecruitPool "barb scythian noblewomen scythian" 0 "requires factions {  scythia, }"
    ]
    ++ zip (repeat TempleOfNaval)
    [ RecruitPool "naval deceres" 0 "requires factions { roman, }"
    , RecruitPool "naval corvus quinquireme" 0 "requires factions { roman, }"
    ]
    ++ zip (repeat TempleOfVictory)
    [ RecruitPool "warband woad briton" 1 "requires factions { britons, }"
    , RecruitPool "warband hurler briton" 1 "requires factions { britons, }"
    ]
    ++ zip (repeat TempleOfViolence)
    [ RecruitPool "barb berserker german" 0 "requires factions { germans, }"
    , RecruitPool "roman arcani" 0 "requires factions { romans_brutii, }"
    ]
    ++ zip (repeat TempleOfViking)
    [ RecruitPool "barb naked fanatics german" 0 "requires factions { germans, }"
    , RecruitPool "barb gothic cavalry german" 0 "requires factions { germans, }"
    ]
