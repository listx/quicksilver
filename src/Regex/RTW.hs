module Regex.RTW where

import Regex
import qualified Data.ByteString.Char8 as BC
import Data.Recruit.RTW

import Util

showResource :: String -> (Int, Int) -> String
showResource res (x, y) = "resource\t" ++ res ++ ",\t" ++ show x ++ ",\t" ++ show y ++ "\r\n"

addResource :: String -> [(Int, Int)] -> BC.ByteString -> BC.ByteString
addResource res coords s = BC.append s $ BC.pack (concatMap (showResource res) coords)

settlementRegionTable :: [(String, String)]
settlementRegionTable =
    [ ("Alesia"            , "Central_Gaul")
    , ("Alexandria"        , "Nile_Delta")
    , ("Ancyra"            , "Galatia")
    , ("Antioch"           , "Syria")
    , ("Apollonia"         , "Epirus")
    , ("Aquincum"          , "Pannonia")
    , ("Ariminum"          , "Umbria")
    , ("Arretium"          , "Etruria")
    , ("Arsakia"           , "Media")
    , ("Artaxarta"         , "Armenia")
    , ("Asturica"          , "Gallaecia")
    , ("Athens"            , "Attica")
    , ("Batavodurum"       , "Germania_Inferior")
    , ("Bordesholm"        , "Tribus_Saxones")
    , ("Bostra"            , "Nabataea")
    , ("Bylazora"          , "Paionia")
    , ("Byzantium"         , "Propontis")
    , ("Campus_Alanni"     , "Tribus_Alanni")
    , ("Campus_Getae"      , "Tribus_Getae")
    , ("Campus_Iazyges"    , "Tribus_Iazyges")
    , ("Campus_Sakae"      , "Tribus_Sakae")
    , ("Campus_Sarmatae"   , "Tribus_Sarmatae")
    , ("Campus_Scythii"    , "Scythia")
    , ("Capua"             , "Campania")
    , ("Caralis"           , "Sardinia")
    , ("Carthage"          , "Africa")
    , ("Carthago_Nova"     , "Hispania")
    , ("Chersonesos"       , "Bosphorus")
    , ("Cirta"             , "Numidia")
    , ("Condate_Redonum"   , "Armorica")
    , ("Corduba"           , "Baetica")
    , ("Corinth"           , "Peloponnesus")
    , ("Croton"            , "Bruttium")
    , ("Cyrene"            , "Cyrenaica")
    , ("Damascus"          , "Coele_Syria")
    , ("Damme"             , "Tribus_Chattii")
    , ("Deva"              , "Tribus_Silurii")
    , ("Dimmidi"           , "Gaetulia")
    , ("Domus_Dulcis_Domus", "Locus_Gepidae")
    , ("Dumatha"           , "Arabia")
    , ("Eburacum"          , "Britannia_Inferior")
    , ("Halicarnasus"      , "Lycia")
    , ("Hatra"             , "Assyria")
    , ("Iuvavum"           , "Noricum")
    , ("Jerusalem"         , "Judaea")
    , ("Kotais"            , "Colchis")
    , ("Kydonia"           , "Crete")
    , ("Larissa"           , "Thessalia")
    , ("Lemonum"           , "Aquitania")
    , ("Lepcis_Magna"      , "Tripolitania")
    , ("Lilybaeum"         , "Sicilia_Poeni")
    , ("Londinium"         , "Britannia_Superior")
    , ("Lovosice"          , "Boihaemum")
    , ("Lugdunum"          , "Lugdinensis")
    , ("Massilia"          , "Transalpine_Gaul")
    , ("Mazaka"            , "Cappadocia")
    , ("Mediolanium"       , "Cisalpine_Gaul")
    , ("Memphis"           , "Middle_Egypt")
    , ("Messana"           , "Sicilia_Romanus")
    , ("Mogontiacum"       , "Agri_Decumates")
    , ("Narbo_Martius"     , "Narbonensis")
    , ("Nepte"             , "Sahara")
    , ("Nicomedia"         , "Bithynia")
    , ("Numantia"          , "Celtiberia")
    , ("Osca"              , "Taraconenis")
    , ("Palma"             , "Baliares")
    , ("Palmyra"           , "Regnum_Palmyrae")
    , ("Patavium"          , "Venetia")
    , ("Pergamum"          , "Phrygia")
    , ("Petra"             , "Sinai")
    , ("Phraaspa"          , "Atropatene")
    , ("Porrolissum"       , "Dacia")
    , ("Rhodes"            , "Rhodos")
    , ("Rome"              , "Latium")
    , ("Salamis"           , "Cyprus")
    , ("Salona"            , "Dalmatia")
    , ("Samarobriva"       , "Belgica")
    , ("Sardis"            , "Ionia")
    , ("Scallabis"         , "Lusitania")
    , ("Segesta"           , "Liguria")
    , ("Segestica"         , "Illyria")
    , ("Seleucia"          , "Babylonia")
    , ("Sidon"             , "Phoenicia")
    , ("Sinope"            , "Pontus")
    , ("Siwa"              , "Libya")
    , ("Sparta"            , "Laconia")
    , ("Susa"              , "Elymais")
    , ("Syracuse"          , "Sicilia_Graecus")
    , ("Tanais"            , "Maeotis")
    , ("Tara"              , "Hibernia")
    , ("Tarentum"          , "Apulia")
    , ("Tarsus"            , "Cilicia")
    , ("Thapsus"           , "Byzacium")
    , ("Thebes"            , "Thebais")
    , ("Themiskyra"        , "Hyperboria")
    , ("Thermon"           , "Aetolia")
    , ("Thessalonica"      , "Macedonia")
    , ("Tingi"             , "Mauretania")
    , ("Trier"             , "Germania_Superior")
    , ("Tylis"             , "Thrace")
    , ("Vicus_Gothi"       , "Locus_Gothi")
    , ("Vicus_Marcomannii" , "Regnum_Marcomannii")
    , ("Vicus_Venedae"     , "Pripet")
    ]

-- Coordinates for extra elephant resources.
elephantCoords :: [(Int, Int)]
elephantCoords =
    [ (52, 92)   -- Nepte         -- merc pool "North Africa"; already has elephant merc
    , (101, 15)  -- Lepcis Magna  -- merc pool "Libya"
    , (135, 16)  -- Cyrene        -- merc pool "Libya"
    , (178, 1)   -- Siwa          -- merc pool "Libya"
    , (183, 16)  -- Alexandria    -- merc pool "Egypt"
    , (182, 7)   -- Memphis       -- merc pool "Egypt"
    , (187, 4)   -- Thebes        -- merc pool "Egypt"
    , (203, 18)  -- Petra         -- merc pool "Syria"; already has elephant merc
    , (198, 23)  -- Jerusalem     -- merc pool "Arabia"
    , (213, 2)   -- Bostra        -- merc pool "Arabia"
    , (237, 23)  -- Dumatha       -- merc pool "Arabia"
    , (214, 48)  -- Palmyra       -- merc pool "Arabia"
    ]

mercPoolsNeedingElephs :: [String]
mercPoolsNeedingElephs = ["Libya", "Egypt", "Arabia"]

_RTW_DC_FUNCS :: RegexSets
_RTW_DC_FUNCS = addTrueTest [r1, r2]
    where
        -- Campaign movement speed 1.75x
        r1 =    [ ("^starting_action_points\\s+", id)
                , (multRoundInt 1.75)
                ]
        r2 =
                -- Assassin upkeep cost 3x
                [ ("^type\\s+assassin\\s+[\\w\\s,]+?\\r\\nwage_base\\s+", id)
                , (multRoundInt 3)
                ]

_RTW_DCL_FUNCS :: RegexSets
_RTW_DCL_FUNCS = addTrueTest [r1, r2, r3]
    where
        -- Diplomats and assassins take 0 turns to recruit
        r1 =    [ ("^diplomat.+?", id)
                , ("1", only "0")
                , (".+?", id)
                , ("1", only "0")
                ]
        r2 =    [ ("^assassin.+?", id)
                , ("1", only "0")
                , (".+?", id)
                , ("1", only "0")
                ]
        -- Assassin recruitment cost 3x
        r3 =    [ ("^assassin.+?", id)
                , (multRoundInt 3)
                ]

_RTW_DMR_FUNCS :: RegexSets
_RTW_DMR_FUNCS = addTrueTest mercPoolAddElephs
    ++ addTrueTest [mercRecruit]
    where
        -- Add elephant mercenary units to elephant regions.
        mercPoolAddElephs = map f mercPoolsNeedingElephs
        f pool =
            [ ("^pool " ++ pool ++ ".+?regions.+?\\r\\n", add mercEleph)
            ]
        mercEleph = "\tunit merc elephants,\t\t\t\texp 0 cost 4000 replenish 0.005 - 0.015 max 1 initial 0\r\n"
        mercRecruit =
                -- Increase costs for mercenary units 1.25x.
                [ ("^\\s+unit.+?cost.+?", id)
                , (multRoundInt 1.25)
                -- Increase merc replenish rate 3x.
                , (".+?replenish.+?", id)
                , (multDouble 3)
                , (" - ", id)
                , (multDouble 3)
                , (".+?max.+?", id)
                -- Increase merc max amount 5x.
                , (multRoundInt 5)
                , (".+?initial.+?", id)
                -- Increase merc initial amount 5x.
                , (multRoundInt 5)
                ]

_RTW_DS_FUNCS :: RegexSets
_RTW_DS_FUNCS = addTrueTest [r1, r2]
    ++ addTrueTest (noSpies)
    ++ mineFuncs capsSecs
    ++ [missingSettlementDescs]
    ++ addTrueTest [giveRoads]
    ++ giveRoads'
    ++ addTrueTest removeExistingOverlappedRes
    ++ addTrueTest goldMod
    ++ addTrueTest [elephantsMod]
    where
        -- Rebel spawn rate 40x lower
        r1 =    [ ("^brigand_spawn_value\\s+", id)
                , (multRoundInt 40)
                ]
        -- Pirate spawn rate 20x lower
        r2 =    [ ("^pirate_spawn_value\\s+", id)
                , (multRoundInt 20)
                ]
        -- Remove all spies from the campaign map.
        noSpies =
            [
                [ ("character[^\\r]+spy.+?\\r\\n.+?\\r\\n", nil)
                ]
            ,
                [ ("^ancillaries catamite\\r\\n", nil)
                ]
            ,
                [ ("^ancillaries courtesan\\r\\n", nil)
                ]
            ,
                [ ("^ancillaries dancer\\r\\n", nil)
                ]
            ,
                [ ("^ancillaries poisoner\\r\\n", nil)
                ]
            ,
                [ ("^ancillaries spymaster\\r\\n", nil)
                ]
            ]
        -- The following 6 settlements are missing from descr_strat.txt, so add
        -- them in to build roads on them:
        --
        --  PROVINCE            REGION
        --  --------            ------
        --  Dumatha             Arabia
        --  Phraaspa            Atropatene
        --  Lovosice            Boihaemum
        --  Salona              Dalmatia
        --  Ancyra              Galatia
        --  Domus_Dulcis_Domus  Locus_Gepidae
        --  Vicus_Venedae       Pripet
        missingSettlementDescs =
                [ ("Tripolitania.+?}\\r\\n", add $ concatMap makeTown sdescs, alwaysTrue)
                ]
            where
                makeTown (province, culture) =
                    case lookup province settlementRegionTable of
                        Just r -> "settlement\r\n{\r\n"
                            ++ "\tlevel town\r\n"
                            ++ "\tregion " ++ r ++ "\r\n\r\n"
                            ++ "\tyear_founded 0\r\n"
                            ++ "\tpopulation 2700\r\n"
                            ++ "\tplan_set default_set\r\n"
                            ++ "\tfaction_creator " ++ culture ++ "\r\n"
                            ++ "\tbuilding\r\n"
                            ++ "\t{\r\n"
                            ++ "\t\ttype core_building governors_house\r\n"
                            ++ "\t}\r\n"
                            ++ "}\r\n"
                        _ -> "" -- don't add anything if we make a human error in defining sdescs
                sdescs =
                    [ ("Dumatha", "parthia")
                    , ("Phraaspa", "parthia")
                    , ("Lovosice", "gauls")
                    , ("Salona", "macedon")
                    , ("Ancyra", "macedon")
                    , ("Domus_Dulcis_Domus", "gauls")
                    , ("Vicus_Venedae", "gauls")
                    ]
        -- Give all settlements paved roads.
        govHouse =
            "\r\n\tbuilding\r\n\
            \\t{\r\n\
            \\t\ttype core_building governors_house\r\n\
            \\t}"
        roads =
            "\r\n\tbuilding\r\n\
            \\t{\r\n\
            \\t\ttype hinterland_roads paved_roads\r\n\
            \\t}"
        giveRoads =
                -- remove all vanilla road buildings
                [ ("^\\s+building\\r\\n\\s+\\{\\r\\n\\s+type\\s+hinterland_roads\\s+roads\\r\\n\\s+\\}\\r\\n", nil)
                ]
        giveRoads' =
            -- add paved_roads
            [
                -- add paved_roads to non-village settlements
                [ ("^settlement\\r\\n.+?level ", id, alwaysTrue)
                , ("\\w+", id, strElemTest $ map BC.pack ["town", "large_town", "city"])
                , (".+?", id, alwaysTrue)
                , ("\\r\\n\\}", prepend roads, alwaysTrue)
                ]
            ,
                -- for villages, convert them to towns + governors_house (the
                -- minimum amount of buildings for a town)
                [ ("^settlement\\r\\n.+?level ", id, alwaysTrue)
                , ("village", only "town", alwaysTrue)
                , (".+?", id, alwaysTrue)
                , ("\\r\\n\\}", prepend (govHouse ++ roads), alwaysTrue)
                ]
            ]
        removeExistingOverlappedRes = map f $ goldCoords ++ silverCoords
            where
                f (x, y) =
                    [ ("^resource[^\\d]+?" ++ show x ++ "[^\\d]+?" ++ show y ++ "\\r\\n", nil)
                    ]
        goldMod =
            [
                -- Replace all gold resources with purple dye in the map
                [ ("^resource\\s+", id)
                , ("gold", only "purple_dye")
                ]
            ,
                -- Replace all silver resources with incense in the the map
                [ ("^resource\\s+", id)
                , ("silver", only "incense")
                ]
            ,
                -- Add in 1 gold resource for every faction's capital city region
                [ ("resources.+?\\r\\n\\r\\n", addResource "gold" goldCoords)
                ]
            ,
                -- Add in 1 silver resource for every faction's second city (or
                -- the capital city if no second city)
                [ ("resources.+?\\r\\n\\r\\n", addResource "silver" silverCoords)
                ]
            ]
        -- Gold resource locations, one for each faction's capital
        goldCoords =
            [ (58, 41) -- Cirta (Numidia)
            , (78, 41) -- Carthage (Carthage)
            , (181, 21) -- Alexandria (Egypt)
            , (204, 53) -- Antioch (Seleucid)
            , (249, 76) -- Arsakia (Parthia)
            , (225, 90) -- Artaxarta (Armenia)
            , (162, 103) -- Campus Scythii (Scythia)
            , (148, 79) -- Tylis (Thrace; same gold as orig)
            , (140, 46) -- Sparta (Greece)
            , (136, 98) -- Porrolissum (Dacia; same gold as orig)
            , (93, 132) -- Damme (Germania)
            , (59, 130) -- Londinium (Britannia)
            , (68, 103) -- Alesia (Gaul)
            , (20, 84) -- Asturica (Spain; same gold vanilla)
            , (87, 80) -- Arretium (Julii)
            , (109, 66) -- Tarentum (Brutii)
            , (105, 66) -- Capua (Scipii)
            , (97, 76) -- Rome (Senate)
            , (98, 74) -- Rome (Senate)
            , (97, 68) -- Rome (Senate)
            ]
        -- Silver resource location, one for each faction's second region
        silverCoords =
            [ (36, 24) -- Dimmidi (Numidia)
            , (81, 35) -- Thapsus (Carthage)
            , (188, 15) -- Memphis (Egypt)
            , (195, 60) -- Tarsus (Seleucid; same silver as orig)
            , (254, 45) -- Susa (Parthia)
            , (210, 94) -- Kotais (Armenia)
            , (185, 110) -- Tanais (Scythia)
            , (152, 99) -- Campus Getae (Thrace)
            , (160, 63) -- Pergamum (Greece; same silver as orig)
            , (132, 108) -- Campus lazyges (Dacia)
            , (95, 111) -- Mogontiacum (Germania)
            , (45, 135) -- Deva (Britannia; gold vanilla)
            , (55, 97) -- Lemonum (Gaul)
            , (6, 68) -- Scallabis (Spain)
            , (99, 77) -- Ariminum (Julii)
            , (112, 55) -- Croton (Brutii; silver vanilla)
            , (105, 48) -- Messana (Scipii)
            ]
        -- Give mines+1 to all factions' capitals/secondary cities, because the AI
        -- is too stupid and doesn't always build mines immediately.
        mine =
            "\r\n\tbuilding\r\n\
            \\t{\r\n\
            \\t\ttype hinterland_mines mines+1\r\n\
            \\t}"
        mineFuncs :: [String] -> RegexSets
        mineFuncs strs = concatMap makeFunc strs
            where
                makeFunc str = case lookup str settlementRegionTable of
                    Just region -> addTrueTest
                        [
                            [ ("region " ++ region ++ ".+?", id)
                            , ("\\r\\n\\}", prepend mine)
                            ]
                        ]
                    _ -> [] -- don't add any regexes if we put in an incorrect settlement name; should never happen except for human error
        capsSecs =
            [ "Cirta"
            , "Carthage"
            , "Alexandria"
            , "Antioch"
            , "Arsakia"
            , "Artaxarta"
            , "Campus"
            , "Tylis"
            , "Sparta"
            , "Porrolissum"
            , "Damme"
            , "Londinium"
            , "Alesia"
            , "Asturica"
            , "Arretium"
            , "Tarentum"
            , "Capua"
            -- , "Rome" Rome already has a mining network in vanilla
            , "Dimmidi"
            , "Thapsus"
            , "Memphis"
            , "Tarsus"
            , "Susa"
            , "Kotais"
            , "Tanais"
            , "Campus"
            , "Pergamum"
            , "Campus"
            , "Mogontiacum"
            , "Deva"
            , "Lemonum"
            , "Scallabis"
            , "Ariminum"
            , "Croton"
            , "Messana"
            ]
        -- Extra elephants locations (put elephants all over northern Africa)
        elephantsMod =
                [ ("resources.+?\\r\\n\\r\\n", addResource "elephants" elephantCoords)
                ]

_RTW_DSN_FUNCS :: RegexSets
_RTW_DSN_FUNCS = addTrueTest missionMoney
    where
        -- Senate mission monetary rewards 3x.
        missionMoney =
            [
                [ ("_reward\\s+", id)
                , (multRoundInt 3)
                ]
            ]

_RTW_DSR_FUNCS :: RegexSets
_RTW_DSR_FUNCS = addTrueTest [r1, r2]
    where
        -- Gold resource worth x5 (to increase mining profit)
        r1 =    [ ("^type\\s+gold\\r\\ntrade_value\\s+", id)
                , (multRoundInt 5)
                ]
        -- Silver resource worth x5
        r2 =    [ ("^type\\s+silver\\r\\ntrade_value\\s+", id)
                , (multRoundInt 5)
                ]

_RTW_EDB_FUNCS :: RegexSets
_RTW_EDB_FUNCS = addTrueTest [r1, r2, r2']
    ++ addTrueTest recruitment
    where
        -- All building constructions take 1 turn
        r1 =    [ ("^\\s+construction\\s+", id)
                , (_REGEX_INT, only "1")
                ]
        -- Increased building cost. The extra costs are graduated, with the following formula:
        --      NEW = OLD + (((OLD - 400)/400) * 400)
        --
        r2 =    [ ("^\\s+cost\\s+", id)
                , (_REGEX_INT, gradCost gradFormula)
                ]
        gradFormula :: Double -> Int
        gradFormula n = round $ n + (((n - 400)/400) * 400)
        -- Make a note about the changes in the EDB file (comments)
        r2' =   [ ("^;This.+?by hand.+?\\r\\n", prepend $ costsDiffNote "Building cost" old new chg)
                ]
            where
                old =   [ 400
                        , 600
                        , 800
                        , 1200
                        , 1600
                        , 2000
                        , 2400
                        , 3200
                        , 3500
                        , 4800
                        , 6400
                        , 9600
                        ]
                new = applyFormula gradFormula old
                chg = getChgFactors new old
        -- For every type of building (barracks, archery range, city hall, etc.), let all levels of
        -- that building be able to recruit all units; no more waiting idly until your city becomes
        -- a Huge City to be able to recruit elite units.
        recruitment =
                [
                    [ ("^building\\s+core_building.+?            \\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Core 0 ++ agents "diplomat")
                    , ("^\\s+}.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Core town ++ agents "diplomat")
                    , ("^\\s+}.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Core largeTown ++ agents "diplomat")
                    , ("^\\s+upgrade_bodyguard.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Core city ++ agents "diplomat")
                    , ("^\\s+upgrade_bodyguard.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Core largeCity ++ agents "diplomat")
                    , ("^\\s+upgrade_bodyguard", id)
                    ]
                ,
                    [ ("^building\\s+barracks.+?            \\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Barracks town)
                    , ("^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Barracks largeTown)
                    , ("^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Barracks city)
                    , ("^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Barracks largeCity)
                    , ("^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Barracks hugeCity)
                    , ("^            }", id)
                    ]
                ,
                    [ ("^building\\s+equestrian.+?            \\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Equestrian largeTown)
                    , ("^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Equestrian city)
                    , ("^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Equestrian largeCity)
                    , ("^\\s+stage_races.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Equestrian hugeCity)
                    , ("^\\s+stage_races", id)
                    ]
                ,
                    [ ("^building\\s+missiles.+?            \\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Missiles largeTown)
                    , ("^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Missiles city)
                    , ("^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Missiles largeCity)
                    , ("^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Missiles hugeCity)
                    , ("^            }", id)
                    ]
                ,
                    [ ("^building\\s+market.+?            \\{", id)
                    , ("\\r\\n", only $ "\r\n" ++ agents "assassin")
                    , ("^\\s+trade_base_income_bonus.+?capability.+?\\{\\r\\n", id)
                    , ("^\\s+agent.+?", only $ agents "assassin")
                    , ("^\\s+trade_base_income_bonus.+?capability.+?\\{\\r\\n", id)
                    , ("^\\s+agent.+?", only $ agents "assassin")
                    , ("^\\s+trade_base_income_bonus.+?capability.+?\\{\\r\\n", id)
                    , ("^\\s+agent.+?", only $ agents "assassin")
                    , ("^\\s+trade_base_income_bonus.+?capability.+?\\{\\r\\n", id)
                    , ("^\\s+agent.+?", only $ agents "assassin")
                    , ("^\\s+trade_base_income_bonus", id)
                    ]
                ,
                    [ ("^building\\s+smith.+?            \\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Smith largeTown)
                    , ("^\\s+weapon_simple.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Smith city)
                    , ("^\\s+weapon_simple.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Smith hugeCity)
                    , ("^\\s+weapon_simple", id)
                    ]
                ,
                    [ ("^building\\s+port.+?            \\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Port largeTown)
                    , ("^\\s+trade_fleet.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Port city)
                    , ("^\\s+trade_fleet.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Port largeCity)
                    , ("^\\s+trade_fleet", id)
                    ]
                ,
                    [ ("^building\\s+amphitheatres.+?            \\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Amphitheatres city)
                    , ("^\\s+stage_games.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Amphitheatres largeCity)
                    , ("^\\s+stage_games.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Amphitheatres hugeCity)
                    , ("^\\s+stage_games", id)
                    ]
                ]
                ++ recruitmentTemples
        recruitmentTemples = map makeRegexSnippets
            [ (TempleOfBattle,      [largeTown, city, largeCity, hugeCity])
            , (TempleOfBattleforge, [largeTown, city, largeCity])
            , (TempleOfFertility,   [town, largeTown, city, largeCity, hugeCity])
            , (TempleOfHealing,     [city, largeCity, hugeCity])
            , (TempleOfJustice,     [largeTown, city, largeCity, hugeCity])
            , (TempleOfLaw,         [largeCity, hugeCity])
            , (TempleOfLeadership,  [largeCity, hugeCity])
            , (TempleOfLove,        [largeTown, city, largeCity, hugeCity])
            , (TempleOfNaval,       [largeCity, hugeCity])
            , (TempleOfVictory,     [town, largeTown, city, largeCity, hugeCity])
            , (TempleOfViolence,    [city, largeCity, hugeCity])
            , (TempleOfViking,      [largeTown, city])
            ]
        makeRegexSnippets (building, levels) = [buildingRegex]
            ++ concatMap makeSnippetInit (init levels)
            ++ makeSnippetLast (last levels)
            where
                buildingRegex =
                    ("^building\\s+" ++ show building ++ "\\r\\n.+?            \\{\\r\\n", id)
                makeSnippetInit level =
                    [ ("\\s+recruit.+?", only $ getRecruits building level)
                    , ("^\\s+happiness_bonus.+?capability.+?\\{\\r\\n", id)
                    ]
                makeSnippetLast level =
                    [ ("\\s+recruit.+?", only $ getRecruits building level)
                    , ("^\\s+happiness_bonus", id)
                    ]
        agents agent = concatMap (\(a, b) -> a ++ "{ " ++ b ++ ", }\r\n") $ zip (repeat agent')
            [ "barbarian"
            , "carthaginian"
            , "eastern"
            -- , "parthia" -- "parthia" is redundant because it's already included under "eastern"
            , "egyptian"
            , "greek"
            , "roman"
            ]
            where
                agent' = (replicate 16 ' ') ++ "agent " ++ agent ++ " 0 requires factions "

_RTW_EDCT_FUNCS :: RegexSets
_RTW_EDCT_FUNCS = addTrueTest [r1, r2]
    where
        -- Give good assassins a line of sight bonus with increased skill.
        r1 =    [ ("^Trait GoodAssassin.+?", id)
                , ("Effect Subterfuge.+?", id)
                , ("\\r\\n", only "\r\n        Effect LineOfSight 3\r\n")
                , (".+?Effect Subterfuge.+?", id)
                , ("\\r\\n", only "\r\n        Effect LineOfSight 5\r\n")
                , (".+?Effect Subterfuge.+?", id)
                , ("\\r\\n", only "\r\n        Effect LineOfSight 8\r\n")
                , (".+?Effect Subterfuge.+?", id)
                , ("\\r\\n", only "\r\n        Effect LineOfSight 9\r\n")
                , (".+?Effect Subterfuge.+?", id)
                , ("\\r\\n", only "\r\n        Effect LineOfSight 10\r\n")
                ]
        -- Remove corruption trigger based on high treasury
        r2 =    [ ("^Trigger corruption.+?;-+", nil)
                ]

_RTW_EDU_FUNCS :: RegexSets
_RTW_EDU_FUNCS = addTrueTest unitTurns ++ addTrueTest [r1] ++ addTrueTest missileInfantryAmmo
    where
        -- All units take 0 turns to recruit, but they cost cost 1.33x, 1.66x,
        -- and 2x more (initial cost) based on their original turn count.
        unitTurns =
            [
            -- First multiply the initial costs appropriately, depending on
            -- original turn count.
                [ ("^stat_cost\\s+", id)
                , ("1,\\s+", id )
                , (multRoundInt 1.33)
                ]
            ,
                [ ("^stat_cost\\s+", id)
                , ("2,\\s+", id )
                , (multRoundInt 1.66)
                ]
            ,
                [ ("^stat_cost\\s+", id)
                , ("3,\\s+", id )
                , (multRoundInt 2)
                ]
            ,
            -- Now set the turn count for all units to 0.
                [ ("^stat_cost\\s+", id)
                , (_REGEX_INT, only "0")
                ]
            ]
        -- General's bodyguard (cavalry) soldiers reduced 0.5x; costs reduced accordingly
        -- NOTE: It's important to do this _after_ setting the global stat
        -- costs (it just makes more sense that way --- do the global settings
        -- first, then do specific customizations such as this).
        r1 =    [ ("^soldier.+?", id) -- we can lead with this generic regex b/c the text is broken up into 1-unit blocks (no danger of matching over into the next unit(s))
                , (multRoundInt 0.5) -- soldiers 0.5x
                , (".+?^attributes.+?general_unit.+?", id)
                , ("^stat_cost\\s+\\d+,\\s+", id) -- skip recruitment turns cost
                , (multRoundInt 0.5) -- recruitment cost
                , (",\\s+", id)
                , (multRoundInt 0.5) -- upkeep cost
                , (",\\s+", id)
                , (multRoundInt 0.5) -- weapon upgrade cost
                , (",\\s+", id)
                , (multRoundInt 0.5) -- armor upgrade cost
                , (",\\s+", id)
                , (multRoundInt 0.5) -- custom battle: recruitment cost
                ]
        -- Missile infantry ammo 1.25x; 1.5x for slingers
        missileInfantryAmmo =
            [
                [ ("^category\\s+infantry\\s+class\\s+missile.+?stat_pri\\s+.+?,.+?,.+?,.+?,\\s+", id)
                , (multRoundInt 1.25)
                ]
            ,
                [ ("^stat_pri[^\\r]+?stone,.+?, ", id)
                , (_REGEX_INT, only "60")
                ]
            ]
