module Data where

import qualified Text.Printf as TP

-- Show a (string) double value, with up to 4 digits after the decimal point.
showD :: Double -> String
showD = TP.printf "%.6f"

_RP0, _RP1, _RP2, _RP3, _RP4, _RP5, _RP6 :: (Int, Double, Int, Int)
_RP0 = (0, 0.00, 0, 1) -- village
_RP1 = (1, 0.10, 1, 2) -- town
_RP2 = (2, 0.10, 2, 3) -- large_town
_RP3 = (3, 0.15, 3, 4) -- city
_RP4 = (4, 0.20, 4, 5) -- large_city ("Minor City" in-game)
_RP5 = (6, 0.30, 6, 7) -- huge_city
_RP6 = (7, 0.35, 7, 9) -- huge_city

-- recruitment:
--      recruit_pool
--      UNIT_TYPE
--    b starting point (units available upon completion of building)
--    g points gained per turn
--    c maximum points possible
--    e starting experience of the unit (0 to 9)
--      restrict to certain factions

_CORE_BUILDING_RECRUITS :: (Int, Double, Int, Int) -> String
_CORE_BUILDING_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Conquistadores\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { spain, portugal, }  and hidden_resource america"
    , "\"Swiss Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { papal_states, } "
    , "\"Hussars\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, hungary, } "
    , "\"Cossack Musketeers\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, }  and event_counter gunpowder_discovered 1 "
    , "\"Dismounted Conquistadores\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { spain, portugal, }  and hidden_resource america"
    , "\"Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.2 + g) ++ "   " ++ show (2 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, Normans, } "
    , "\"Italian Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.2 + g) ++ "   " ++ show (2 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, papal_states, sicily, } "
    , "\"EE Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.2 + g) ++ "   " ++ show (2 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, hungary, } "
    , "\"EE Archer Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.2 + g) ++ "   " ++ show (2 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"SE Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.2 + g) ++ "   " ++ show (2 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.2 + g) ++ "   " ++ show (2 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, turks, mongols, timurids, } "
    , "\"ME Archer Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.2 + g) ++ "   " ++ show (2 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Peasant Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.2 + g) ++ "   " ++ show (2 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Saxons, } "
    , "\"Spear Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, Normans, } "
    , "\"Italian Spear Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, papal_states, sicily, } "
    , "\"EE Spear Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, hungary, } "
    , "\"SE Spear Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Spear Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, mongols, timurids, } "
    , "\"Archer Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Scots Pike Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Crossbow Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, denmark, spain, portugal, } "
    , "\"Genoese Crossbow Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, } "
    , "\"Pavise Crossbow Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { venice, papal_states, sicily, hungary, } "
    , "\"EE Crossbow Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, } "
    , "\"S Archer Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Crossbow Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Saracen Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, turks, } "
    , "\"Sabadar Militia\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    , "\"Mongol Horse Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Mounted Sergeants\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Normans, } "
    , "\"Theigns\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Saxons, } "
    , "\"Dismounted Broken Lances\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, } "
    , "\"Noble Pikemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Scots Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Christian Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Broken Lances\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, papal_states, } "
    ]

_CORE_CASTLE_BUILDING_RECRUITS :: (Int, Double, Int, Int) -> String
_CORE_CASTLE_BUILDING_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Dismounted Polish Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Khan's Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"Noble Swordsmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Noble Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Imperial Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, } "
    , "\"Chivalric Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, sicily, } "
    , "\"Italian MAA\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, papal_states, } "
    , "\"Polish Retainers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"E Chivalric Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Byzantine Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Granadine Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Mongol Heavy Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"Feudal Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, denmark, } "
    , "\"Polish Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Boyar Sons\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Vardariotai\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Granadine Jinetes\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Mamluks\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Sipahi Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"Mongol Heavy Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"Dismounted Polish Nobles\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Mailed Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Normans, } "
    , "\"English Huscarls\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Saxons, } "
    , "\"Hobilars\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Border Horse\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Mounted Sergeants\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, milan, venice, papal_states, sicily, Normans, } "
    , "\"Scouts\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Jinetes\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, portugal, } "
    , "\"Polish Shooters\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Kazaks\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Magyar Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Skythikon\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Desert Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Arab Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Turkomans\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"Mongol Horse Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Turkish Horse Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    , "\"Theigns\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Saxons, } "
    , "\"Peasants\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { england, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, } "
    , "\"Highland Rabble\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Southern Peasants\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, byzantium, } "
    , "\"EE Peasants\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, hungary, } "
    , "\"ME Peasants\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, mongols, timurids, } "
    , "\"Levy Spearmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Highlanders\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Sergeant Spearmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, milan, venice, papal_states, sicily, Normans, } "
    , "\"Viking Raiders\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Javelinmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, } "
    , "\"Lusitanian Javelinmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { portugal, } "
    , "\"Woodsmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, } "
    , "\"Slav Levies\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Byzantine Spearmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Berber Spearmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Kurdish Javelinmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Turkish Javelinmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"ME Levy Spearmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Afghan Javelinmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    , "\"Peasant Spearmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { Saxons, } "
    , "\"Peasant Archers\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, } "
    , "\"S Peasant Archers\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, hungary, byzantium, } "
    , "\"EE Peasant Archers\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, } "
    , "\"Sudanese Javelinmen\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"ME Peasant Archers\"  " ++ show (0 + b) ++ "   " ++ showD (0.000001) ++ "   " ++ showD (0.999) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, turks, mongols, timurids, } "
    , "\"Dismounted Feudal Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, denmark, } "
    , "\"Dismounted Noble Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Dismounted Imperial Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, } "
    , "\"Dismounted Chivalric Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, } "
    , "\"Dismounted Italian MAA\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, papal_states, } "
    , "\"Dismounted Norman Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, } "
    , "\"Dismounted Boyar Sons\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Dismounted E Chivalric Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Dismounted Arab Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, } "
    , "\"Dismounted Sipahi Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"Dismounted Heavy Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"Dismounted English Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Dismounted Portuguese Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { portugal, } "
    , "\"Dismounted Feudal Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, france, hre, spain, portugal, milan, venice, papal_states, hungary, } "
    , "\"Arab Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Mamluk Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Sipahis\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"Mongol Light Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Turkomans\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    , "\"Mailed Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, } "
    , "\"Feudal Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, spain, milan, venice, papal_states, } "
    , "\"Dismounted Huscarls\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Norman Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, } "
    , "\"Polish Nobles\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Druzhina\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Hungarian Nobles\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Byzantine Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Mailed Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, spain, portugal, milan, venice, papal_states, sicily, } "
    , "\"Feudal Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    ]

_EQUESTRIAN_RECRUITS :: (Int, Double, Int, Int) -> String
_EQUESTRIAN_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Gothic Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, } "
    , "\"Stradiots\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { venice, } "
    , "\"Polish Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Tsars Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Royal Banderium\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Kataphractoi\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Christian Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Royal Mamluks\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Quapukulu\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"Khan's Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"Reiters\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, }  and event_counter gunpowder_discovered 1 "
    , "\"English Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Chivalric Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, denmark, spain, sicily, } "
    , "\"Imperial Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, } "
    , "\"Portuguese Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { portugal, } "
    , "\"Italian MAA\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, papal_states, } "
    , "\"Polish Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Dvor Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"E Chivalric Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Latinkon\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Granadine Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Mamluks\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Sipahi Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"Mongol Heavy Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"Feudal Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { scotland, } "
    , "\"Druzhina\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Byzantine Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Granadine Jinetes\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Mamluk Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Sipahis\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"Mongol Heavy Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"Mailed Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Boyar Sons\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Byzantine Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Arab Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, } "
    , "\"Turkomans\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, timurids, } "
    , "\"Mongol Light Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Hobilars\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Border Horse\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Mounted Sergeants\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, milan, venice, papal_states, sicily, } "
    , "\"Scouts\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Jinetes\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, portugal, } "
    , "\"Polish Shooters\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Kazaks\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Magyar Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Skythikon\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Desert Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, } "
    , "\"Turkish Horse Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, timurids, } "
    , "\"Mongol Horse Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Mailed Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Normans, } "
    , "\"Hussars\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Cossack Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Elephants\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    , "\"Elephant Artillery\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    , "\"Feudal Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, france, hre, denmark, spain, portugal, milan, venice, papal_states, hungary, } "
    , "\"Norman Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, } "
    , "\"Mailed Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, france, hre, spain, portugal, milan, venice, papal_states, sicily, } "
    , "\"Huscarls\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Polish Nobles\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Hungarian Nobles\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Mounted Sergeants\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, } "
    , "\"Mailed Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Normans, } "
    ]

_BARRACKS_RECRUITS :: (Int, Double, Int, Int) -> String
_BARRACKS_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Musketeers\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, portugal, milan, venice, }  and event_counter gunpowder_discovered 1 "
    , "\"Janissary Musketeers\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, }  and event_counter gunpowder_discovered 1 "
    , "\"Arquebusiers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, france, hre, denmark, spain, milan, venice, papal_states, sicily, poland, russia, }  and event_counter gunpowder_discovered 1 "
    , "\"Portuguese Arquebusiers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { portugal, }  and event_counter gunpowder_discovered 1 "
    , "\"Sudanese Gunners\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, }  and event_counter gunpowder_discovered 1 "
    , "\"Janissary Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"Pikemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Heavy Bill Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Pike Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, spain, portugal, milan, venice, papal_states, sicily, } "
    , "\"Hand Gunners\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, poland, }  and event_counter gunpowder_discovered 1 "
    , "\"Berdiche Axemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Arquebusiers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, }  and event_counter gunpowder_discovered 1 "
    , "\"Varangian Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Hand Gunners\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, turks, timurids, }  and event_counter gunpowder_discovered 1 "
    , "\"Tabardariyya\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Hand Gunners\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, spain, portugal, milan, venice, papal_states, sicily, }  and event_counter gunpowder_discovered 1 "
    , "\"Bill Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Heavy Pike Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Partisan Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Halberd Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, papal_states, sicily, poland, hungary, } "
    , "\"Swordstaff Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Swordsmen Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, portugal, } "
    , "\"Italian Cavalry Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, } "
    , "\"EE Cavalry Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Byzantine Infantry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Urban Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"ME Halberd Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, turks, timurids, } "
    , "\"Archer Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Scots Pike Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Crossbow Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, denmark, spain, portugal, } "
    , "\"Genoese Crossbow Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, } "
    , "\"Pavise Crossbow Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { venice, papal_states, sicily, hungary, } "
    , "\"EE Crossbow Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, } "
    , "\"S Archer Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Crossbow Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Saracen Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, turks, } "
    , "\"Sabadar Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    , "\"Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, Normans, } "
    , "\"Italian Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, papal_states, sicily, } "
    , "\"EE Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, hungary, } "
    , "\"SE Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, timurids, } "
    , "\"Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, Normans, } "
    , "\"Italian Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, papal_states, sicily, } "
    , "\"EE Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, hungary, } "
    , "\"EE Archer Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"SE Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, turks, timurids, } "
    , "\"ME Archer Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Peasant Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Saxons, } "
    , "\"ME Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { mongols, } "
    , "\"Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Normans, } "
    , "\"ME Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Town Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Normans, } "
    , "\"Peasant Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Saxons, } "
    ]

_CASTLE_BARRACKS_RECRUITS :: (Int, Double, Int, Int) -> String
_CASTLE_BARRACKS_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Noble Swordsmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Pikemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Dismounted Broken Lances\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, } "
    , "\"Dismounted Christian Guard\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Heavy Billmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Noble Pikemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Obudshaer\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Armored Swordsmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Highland Pikemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Voulgier\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Zweihander\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, } "
    , "\"Norse Axemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Sword and Buckler Men\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, sicily, } "
    , "\"Aventuros\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { portugal, } "
    , "\"Venetian Heavy Infantry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { venice, } "
    , "\"Dismounted Druchima\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Pavise Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { hungary, } "
    , "\"Dismounted Latinkon\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Lamtuna Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { moors, } "
    , "\"Naffatun\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { egypt, turks, } "
    , "\"Naffatun\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"Dismounted Huscarls\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Billmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Highland Nobles\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Armored Sergeants\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { france, hre, milan, venice, papal_states, sicily, } "
    , "\"Norse Swordsmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Almughavars\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { spain, portugal, } "
    , "\"EE Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { poland, russia, } "
    , "\"Croat Axemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Dismounted Byzantine Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Nubian Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { moors, egypt, } "
    , "\"Azabs\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { turks, } "
    , "\"Mongol Infantry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { mongols, } "
    , "\"Afghan Javelinmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { timurids, } "
    , "\"Armored Sergeants\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Normans, } "
    , "\"Levy Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Highlanders\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Sergeant Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { france, hre, milan, venice, papal_states, sicily, } "
    , "\"Viking Raiders\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Javelinmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { spain, } "
    , "\"Lusitanian Javelinmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { portugal, } "
    , "\"Woodsmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, } "
    , "\"Slav Levies\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Byzantine Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (3 + e) ++ "  requires factions { byzantium, } "
    , "\"Berber Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { moors, } "
    , "\"Kurdish Javelinmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { egypt, } "
    , "\"Turkish Javelinmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { turks, } "
    , "\"ME Levy Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { mongols, } "
    , "\"Sergeant Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Normans, } "
    , "\"Peasant Spearmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { Saxons, } "
    , "\"Peasants\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, } "
    , "\"Highland Rabble\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Southern Peasants\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, byzantium, } "
    , "\"EE Peasants\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, hungary, } "
    , "\"ME Peasants\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, mongols, timurids, } "
    ]

_PROFESSIONAL_MILITARY_RECRUITS :: (Int, Double, Int, Int) -> String
_PROFESSIONAL_MILITARY_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Gendarmes\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, spain, } "
    , "\"Demi Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { england, } "
    , "\"French Mounted Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Tercio Pikemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, } "
    ]

_MISSILES_RECRUITS :: (Int, Double, Int, Int) -> String
_MISSILES_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Dismounted Longbowmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Aventurier\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Mounted Crossbowmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, denmark, spain, portugal, milan, venice, papal_states, sicily, } "
    , "\"Granadine CB Cav\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Yeoman Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Noble Highland Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Crossbowmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, denmark, hungary, } "
    , "\"Pavise Crossbowmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, spain, portugal, papal_states, sicily, } "
    , "\"Genoese Crossbowmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, } "
    , "\"Venetian Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { venice, } "
    , "\"Lithuanian Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Byzantine Guard Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Peasant Crossbowmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Nubian Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { egypt, } "
    , "\"Ottoman Infantry\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { turks, } "
    , "\"Longbowmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Highland Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Peasant Crossbowmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, spain, portugal, milan, venice, papal_states, } "
    , "\"Norse Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Sicilian Muslim Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, } "
    , "\"Lithuanian Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"Bosnian Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"Trebizond Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"Desert Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, } "
    , "\"Turkish Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, timurids, } "
    , "\"Mongol Foot Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Peasant Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, } "
    , "\"S Peasant Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, hungary, byzantium, } "
    , "\"EE Peasant Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"EE Peasant Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { russia, } "
    , "\"Sudanese Javelinmen\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"ME Peasant Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, turks, mongols, timurids, } "
    ]

_SIEGE_RECRUITS :: (Int, Double, Int, Int) -> String
_SIEGE_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"NE Trebuchet\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, } "
    , "\"EE Trebuchet\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, hungary, } "
    , "\"GR Trebuchet\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Trebuchet\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, } "
    , "\"AS Trebuchet\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"NE Catapult\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, Normans, } "
    , "\"EE Catapult\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, hungary, } "
    , "\"GR Catapult\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Catapult\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, } "
    , "\"AS Catapult\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    , "\"NE Ballista\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, Normans, } "
    , "\"EE Ballista\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, hungary, } "
    , "\"GR Ballista\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Ballista\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, } "
    , "\"AS Ballista\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    ]

_CANNON_RECRUITS :: (Int, Double, Int, Int) -> String
_CANNON_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"NE Basilisk\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, spain, portugal, } "
    , "\"NE Monster Ribault\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, } "
    , "\"EE Basilisk\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, hungary, } "
    , "\"ME Monster Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"NE Culverin\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, spain, portugal, milan, venice, papal_states, } "
    , "\"NE Cannon\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, sicily, } "
    , "\"NE Serpentine\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"EE Serpentine\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, hungary, } "
    , "\"EE Cannon\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"ME Cannon\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, } "
    , "\"AS Cannon\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    , "\"NE Serpentine\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, } "
    , "\"NE Cannon\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"EE Cannon\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, } "
    , "\"NE Mortar\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, venice, papal_states, sicily, } "
    , "\"NE Grand Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, spain, portugal, milan, } "
    , "\"NE Ribault\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"EE Ribault\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, hungary, } "
    , "\"EE Grand Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"ME Grand Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, } "
    , "\"AS Grand Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    , "\"NE Ribault\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, spain, portugal, milan, venice, papal_states, sicily, } "
    , "\"NE Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, } "
    , "\"EE Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { poland, russia, hungary, } "
    , "\"GR Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"ME Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, turks, } "
    , "\"AS Rocket Launcher\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"AS Bombard\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { timurids, } "
    ]

_URBAN_EQUESTRIAN_RECRUITS :: (Int, Double, Int, Int) -> String
_URBAN_EQUESTRIAN_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Arab Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, egypt, } "
    , "\"Turkomans\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, timurids, } "
    , "\"Mongol Light Lancers\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Granadine Jinetes\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Mamluk Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, } "
    , "\"Sipahis\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    , "\"Mongol Heavy Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, timurids, } "
    ]

_PORT_RECRUITS :: (Int, Double, Int, Int) -> String
_PORT_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"cog\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, france, scotland, england, } "
    , "\"longboat\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"holk\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, scotland, france, england, } "
    , "\"dragon boat\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"gun holk\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { northern_european, } "
    , "\"carrack\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { northern_european, } and event_counter world_is_round 1 "
    , "\"dhow\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { middle_eastern, } "
    , "\"war galley\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, moors, turks, } "
    , "\"lanternas\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, moors, turks, } "
    , "\"baghlah\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, moors, turks, } and event_counter world_is_round 1 "
    , "\"cog\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, poland, } "
    , "\"ladya\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"war galley\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"holk\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, poland, } "
    , "\"gun holk\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, poland, } "
    , "\"lanternas\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    , "\"carrack\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { eastern_european, } and event_counter world_is_round 1 "
    , "\"dromon\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { greek, } "
    , "\"fire ship\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { greek, } "
    , "\"lanternas\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { greek, } "
    , "\"carrack\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { greek, } and event_counter world_is_round 1 "
    , "\"galley\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { sicily, papal_states, milan, venice, } "
    , "\"cog\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { portugal, spain, } "
    , "\"war galley\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { southern_european, } "
    , "\"lanternas\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { portugal, spain, papal_states, milan, sicily, } "
    , "\"caravel\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { portugal, spain, } and event_counter world_is_round 1 "
    , "\"galleass\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { venice, } "
    , "\"carrack\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { papal_states, sicily, milan, venice, } and event_counter world_is_round 1 "
    , "\"grande carrack\"  " ++ show (1 + b) ++ "   " ++ showD (1 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { portugal, spain, } and event_counter world_is_round 1 "
    ]

_TAVERNS_RECRUITS :: (Int, Double, Int, Int) -> String
_TAVERNS_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Transilvanian Peasants\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hungary, } "
    ]

_CITY_HALL_RECRUITS :: (Int, Double, Int, Int) -> String
_CITY_HALL_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Forlorn Hope\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, } "
    , "\"Carroccio Standard M\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, } "
    , "\"Carroccio Standard V\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { venice, } "
    , "\"Famiglia Ducale\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, } "
    , "\"Janissary Heavy Inf\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { turks, } "
    ]

_BULLRING_RECRUITS :: (Int, Double, Int, Int) -> String
_BULLRING_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Jinetes\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, portugal, } "
    ]

_CARAVAN_RECRUITS :: (Int, Double, Int, Int) -> String
_CARAVAN_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Tuareg Camel Spearmens\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    , "\"Camel Gunners\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, }  and event_counter gunpowder_discovered 1 "
    ]

_GUILD_ASSASSINS_GUILD_RECRUITS :: (Int, Double, Int, Int) -> String
_GUILD_ASSASSINS_GUILD_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Battlefield Assassins\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { hungary, } "
    ]

_GUILD_ASSASSINS_MUSLIM_GUILD_RECRUITS :: (Int, Double, Int, Int) -> String
_GUILD_ASSASSINS_MUSLIM_GUILD_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Hashishim\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (2 + e) ++ "  requires factions { moors, egypt, turks, } "
    ]

-- Note: there is some ugly, hacky (though harmless) code in the vanilla M2TW; we can't just copy
-- the guild HQ's units over here b/c that would leave out some other units, like Sowrdsmen Militia
_GUILD_MASONS_GUILD_RECRUITS :: (Int, Double, Int, Int) -> String
_GUILD_MASONS_GUILD_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Berdiche Axemen\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Bill Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Byzantine Infantry\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { byzantium, } "
    , "\"EE Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { russia, } "
    , "\"Halberd Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { hre, papal_states, sicily, poland, hungary, } "
    , "\"Heavy Bill Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    , "\"Heavy Pike Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { scotland, } "
    , "\"Italian Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { milan, venice, } "
    , "\"ME Halberd Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { egypt, turks, timurids, } "
    , "\"ME Spear Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { mongols, } "
    , "\"Partisan Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, } "
    , "\"Pike Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { france, hre, spain, portugal, milan, venice, papal_states, sicily, } "
    , "\"Swordsmen Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { spain, portugal, } "
    , "\"Swordstaff Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { denmark, } "
    , "\"Urban Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { moors, } "
    ]

-- Give +1 XP for England's Merchant Cavalry Militia as well, just like everyone else. (Bug in M2TW
-- 1.3).
_GUILD_MERCHANTS_GUILD_RECRUITS :: (Int, Double, Int, Int) -> String
_GUILD_MERCHANTS_GUILD_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Merchant Cavalry Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { england, scotland, france, hre, denmark, spain, portugal, poland, hungary, } "
    , "\"Italian Cavalry Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { milan, venice, papal_states, sicily, } "
    , "\"EE Cavalry Militia\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { russia, } "
    , "\"Greek Militia Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { byzantium, } "
    , "\"Arab Cavalry\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { moors, egypt, } "
    , "\"Sipahis\"  " ++ show (1 + b) ++ "   " ++ showD (0.5 + g) ++ "   " ++ show (4 + c) ++ "  " ++ show (1 + e) ++ "  requires factions { turks, } "
    ]

_GUILD_TEMPLARS_CHAPTER_HOUSE_RECRUITS :: (Int, Double, Int, Int) -> String
_GUILD_TEMPLARS_CHAPTER_HOUSE_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Knights Templar\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (2 + e) ++ "  requires factions { england, scotland, france, denmark, milan, venice, papal_states, sicily, poland, hungary, } "
    ]

_GUILD_ST_JOHNS_CHAPTER_HOUSE_RECRUITS :: (Int, Double, Int, Int) -> String
_GUILD_ST_JOHNS_CHAPTER_HOUSE_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Knights Hospitaller\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (2 + e) ++ "  requires factions { england, scotland, france, denmark, milan, venice, papal_states, sicily, poland, hungary, } "
    ]

_GUILD_TEUTONIC_KNIGHTS_CHAPTER_HOUSE_RECRUITS :: (Int, Double, Int, Int) -> String
_GUILD_TEUTONIC_KNIGHTS_CHAPTER_HOUSE_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Teutonic Knights\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (2 + e) ++ "  requires factions { hre, } "
    ]

_GUILD_KNIGHTS_OF_SANTIAGO_CHAPTER_HOUSE_RECRUITS :: (Int, Double, Int, Int) -> String
_GUILD_KNIGHTS_OF_SANTIAGO_CHAPTER_HOUSE_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Knights of Santiago\"  " ++ show (1 + b) ++ "   " ++ showD (0.7 + g) ++ "   " ++ show (6 + c) ++ "  " ++ show (2 + e) ++ "  requires factions { spain, portugal, } "
    ]

_GUILD_WOODSMENS_GUILD_RECRUITS :: (Int, Double, Int, Int) -> String
_GUILD_WOODSMENS_GUILD_RECRUITS (b, g, c, e) = concatMap (\s -> "                recruit_pool " ++ s ++ "\r\n")
    [ "\"Sherwood Archers\"  " ++ show (1 + b) ++ "   " ++ showD (0.4 + g) ++ "   " ++ show (3 + c) ++ "  " ++ show (0 + e) ++ "  requires factions { england, } "
    ]
