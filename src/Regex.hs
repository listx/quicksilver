module Regex where

import Data.Array
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BC
import Text.Regex.PCRE hiding (match)
import qualified Text.Printf as TP

import Option
import Util

_REGEX_INT = "\\d+"
_REGEX_DOUBLE = "\\d+\\.\\d+"

_DC_FUNCS = addTrueTest [r1, r2, r3, r4, rN]
    where
        -- Ship movement speed 3x
        r1 =    [ ("^type\\s+admiral.+?starting_action_points\\s+", id)
                , (multRoundInt 3)
                ]
        -- Diplomat movement speed 2x
        r2 =    [ ("^type\\s+diplomat.+?starting_action_points\\s+", id)
                , (multRoundInt 2)
                ]
        -- Princess movement speed 1.75x
        r3 =    [ ("^type\\s+princess.+?starting_action_points\\s+", id)
                , (multRoundInt 1.75)
                ]
        -- Campaign movement speed 1.75x
        r4 =    [ ("^starting_action_points\\s+", id)
                , (multRoundInt 1.75)
                ]
        -- Spy upkeep cost 3x
        rN =    [ ("^type\\s+spy\\s+[\\w\\s,]+?\\r\\nwage_base\\s+", id)
                , (multRoundInt 3)
                ]

_DCD_FUNCS = addTrueTest [r1, r2, r3, r4, r5, r6, r7, r8, r9]
    where
        -- Reduce random heretic/witch spawn rate by 75%
        r1 =    [ ("witch_creation_modifier.+?\"", id)
                , (multDouble 0.25)
                ]
        r2 =    [ ("heretic_creation_modifier.+?\"", id)
                , (multDouble 0.25)
                ]
        -- Reduce heretic/witch max (allowed on campaign map) by 75%
        r3 =    [ ("max_witches uint.+?\"", id)
                , (multRoundInt 0.25)
                ]
        r4 =    [ ("max_heretics uint.+?\"", id)
                , (multRoundInt 0.25)
                ]
        -- Reduce inquisitor denounce success rate by 50%
        r5 =    [ ("denounce_inquisitor_base_chance.+?\"", id)
                , (multDouble 0.5)
                ]
        -- Improve priest denouncing power (against heretics/witches) 2x
        r6 =    [ ("denounce_priest_base_chance.+?\"", id)
                , (multDouble 2)
                ]
        -- Improve effectiveness of assassins 2x
        r7 =    [ ("assassinate_base_chance.+?\"", id)
                , (multRoundInt 2)
                ]
        -- Assassin's minimum success chance changed from 5% to 17% (1 out of 6, just like rolling a
        -- die; so a newbie assassin should be able to succeed more easily), and maximum chance
        -- changed from 95% to 99%
        r8 =    [ ("assassinate_chance_min.+?\"", id)
                , (_REGEX_INT, only "17")
                ]
        r9 =    [ ("assassinate_chance_max.+?\"", id)
                , (_REGEX_INT, only "99")
                ]

_DCAD_FUNCS = addTrueTest [r1]
    where
        -- Stop Papal States from capturing rebel settlements (helps land-strapped factions like Milan and Sicily expand into Africa more easily)
        r1 =    [ ("papal_faction.+?invasion_decisions.+?", id)
                , ("<decision_entry>.+?", id)
                , ("<decision_entry>.+?", id)
                , ("<decision_entry>.+?", id)
                , ("<decision_entry>.+?", id)
                , ("<decision_entry>.+?", id)
                , ("<decision_entry>.+?", id)
                , ("<decision_entry>.+?", id)
                , ("<decision_entry>.+?", id)
                , ("<decision_entry>.+?", id)
                , ("\\r\\n\\s+<decision_entry>.+?", nil)
                , ("<decision_entry>.+?", nil)
                , ("<decision_entry>.+?", nil)
                , ("<decision_entry>.+?", nil)
                , ("\\r\\n\\s+<decision_entry>", id)
                ]

_DCL_FUNCS = addTrueTest [rN]
    where
        -- Spy recruitment cost 3x
        rN =    [ ("^spy.+?spy\\.tga\\s+", id)
                , (multRoundInt 3)
                ]

_DFS_FUNCS = addTrueTest [rN]
    where
        -- Fixed faction standing bug
        rN =    [ ("^;Trigger 0102_city_razed.+?;-+", nil)
                ]

_DM_FUNCS = addTrueTest [r1, r2, r3, r4a, r4b, r4c, r5, r6a, r6b, r4']
    where
        -- Disable all mission penalties, and disable the "cease hostilities" mission
        r1 =    [ ("^\\s+penalty\\r\\n.+?\\}\\r\\n", nil)
                ]
        r2 =    [ ("^mission\\s+cease.+?^\\}\\r\\n\\r\\n\\r\\n", nil)
                ]
        -- Fix guild_assassin_payback bug (10 gold (obvious typo) instead of 100 gold)
        r3 =    [ ("guild_money\\s+", id)
                , (_REGEX_INT, only "100")
                , ("\\s+assassins_guild", id)
                ]
        -- Increase all missions' monetary rewards with this formula:
        --      NEW = OLD + (((OLD - 100)/100) * 2000)
        --
        r4a =   [ ("^\\s+money\\s+", id)
                , (_REGEX_INT, gradCost moneyFormula)
                ]
        r4b =   [ ("^\\s+guild_money\\s+", id)
                , (_REGEX_INT, gradCost moneyFormula)
                ]
        -- Fix council_min/mod_money bug (the lines starting with "    cash ..." were not correctly
        -- mapped to their intended paybacks)
        --
        -- Also, increase the cash threshold 10x for triggering the major/mod/min money rewards
        r4c =   [ ("^\\s+cash\\s+", id)
                , (multRoundInt 10)
                , ("\\s+payback_id\\s+", id)
                , ("council_min_money", only "council_mod_money")
                , ("\\s+^\\s+cash\\s+", id)
                , (multRoundInt 10)
                , ("\\s+payback_id\\s+", id)
                , ("council_mod_money", only "council_min_money")
                ]
        moneyFormula :: Double -> Int
        moneyFormula n = round $ n + (((n - 100)/100) * 2000)
        -- Make a note about the changes in the DM file (comments)
        r4' =   [ ("^; descr_missions.+?\\r\\n", prepend $ costsDiffNote "Mission money reward" old new chg)
                ]
            where
                old =   [ 100
                        , 200
                        , 300
                        , 500
                        , 1000
                        , 2000
                        , 2500
                        , 3000
                        , 5000
                        ]
                new = applyFormula moneyFormula old
                chg = getChgFactors new old
        -- Increase every mission's military rewards 3x
        r5 =    [ ("_unit\\s+\\d+\\s+", id)
                , (multRoundInt 3)
                ]
        -- Decrease mission durations (except for the "convert" mission where you have to convert a
        -- settlement's religion)
        r6a =   [ ("\\s+duration\\s+", id)
                , (multRoundInt 0.66)
                ]
        -- Change the 'convert' mission's duration back to 15
        r6b =   [ ("^mission convert.+?duration\\s+", id)
                , (_REGEX_INT, only "15")
                ]

_DS_FUNCS = addTrueTest [r1, r2, r3, r4, r5, r6, r7, r8] ++ [r9, r10, r11, r12]
    where
        -- Rebel spawn rate 10x lower
        r1 =    [ ("^brigand_spawn_value\\s+", id)
                , (multRoundInt 10)
                ]
        -- Pirate spawn rate 10x lower
        r2 =    [ ("^pirate_spawn_value\\s+", id)
                , (multRoundInt 10)
                ]
        -- King's purse 2x
        r3 =    [ ("^denari_kings_purse\\s+", id)
                , (multRoundInt 2)
                ]
        -- Replace all mineable resources from the map, except for gold and silver, with similarly
        -- valued non-mineable resources; this does two things:
        --  (1) only allow gold and silver to be mineable
        --  (1) reduce the needlessly large variety of resources
        r4 =    [ ("^resource\\s+", id)
                , ("tin", only "wool")
                ]
        r5 =    [ ("^resource\\s+", id)
                , ("iron", only "wine")
                ]
        r6 =    [ ("^resource\\s+", id)
                , ("sulfur", only "grain")
                ]
        r7 =    [ ("^resource\\s+", id)
                , ("marble", only "sugar")
                ]
        r8 =    [ ("^resource\\s+", id)
                , ("coal", only "furs")
                ]
        -- Remove all gold resources from the map, except for Aztecs
        r9 =    [ ("^resource\\s+gold,\\s+", nil, alwaysTrue)
                , (_REGEX_INT, nil, numTest (>= 30))
                , (",\\s+\\d+\\r\\n", nil, alwaysTrue)
                ]
        -- Remove all silver resources from the map, except for Aztecs
        r10 =   [ ("^resource\\s+silver,\\s+", nil, alwaysTrue)
                , (_REGEX_INT, nil, numTest (>= 30))
                , (",\\s+\\d+\\r\\n", nil, alwaysTrue)
                ]
        -- Add in 1 gold resource for every faction's capital city region
        r11 =   [ ("^resource\\s+gold,\\s+5,\\s+143\\r\\n", addResource "gold" goldCoords, alwaysTrue)
                ]
        -- Add in 1 silver resource for every faction's second city (or the capital city if no
        -- second city)
        r12 =   [ ("^resource\\s+silver,\\s+4,\\s+123\\r\\n", addResource "silver" silverCoords, alwaysTrue)
                ]
        showResource :: String -> (Int, Int) -> String
        showResource res (x, y) = "resource\t" ++ res ++ ",\t" ++ show x ++ ",\t" ++ show y ++ "\r\n"
        addResource :: String -> [(Int, Int)] -> BC.ByteString -> BC.ByteString
        addResource res coords s = BC.append s $ BC.pack (concatMap (showResource res) coords)
        -- Gold resource locations, one for each faction's capital (except Aztecs)
        goldCoords =
            [ (97,174) -- Scotland
            , (93, 145) -- England
            , (114, 120) -- France
            , (66, 105) -- Spain
            , (57, 81) -- Portugal
            , (69, 74) -- Moors
            , (127, 107) -- Milan
            , (151, 66) -- Sicily
            , (148, 93) -- Papal States (Rome region)
            , (145, 91) -- Papal States (Rome region)
            , (147, 88) -- Papal States (Rome region)
            , (147, 94) -- Papal States (Rome region)
            , (149, 86) -- Papal States (Rome region)
            , (144, 92) -- Papal States (Rome region)
            , (149, 92) -- Papal States (Rome region)
            , (153, 109) -- Venice
            , (142, 138) -- HRE
            , (139, 168) -- Denmark
            , (186, 136) -- Poland
            , (176, 123) -- Hungary
            , (207, 90) -- Byzantium
            , (206, 178) -- Russia
            , (231, 81) -- Turks
            , (239, 30) -- Egypt
            ]
        -- Silver resource location, one for each faction's second region
        silverCoords =
            [ (101,162) -- Scotland
            , (103, 156) -- England
            , (121, 127) -- France
            , (77, 94) -- Spain
            , (88, 104) -- Portugal
            , (73, 73) -- Moors
            , (131, 106) -- Milan
            , (161, 82) -- Sicily
            , (166, 99) -- Venice
            , (151, 128) -- HRE
            , (142, 171) -- Denmark
            , (194, 136) -- Poland
            , (191, 119) -- Hungary
            , (207, 82) -- Byzantium
            , (217, 182) -- Russia
            , (246, 91) -- Turks
            , (251, 37) -- Egypt
            ]

_DSK_FUNCS = addTrueTest [r1, r2, r3]
    where
        -- Diplomats/princesses: remove annoying "conduct diplomacy" animation
        r1 =    [ ("^anim\\s+conduct_diplomacy.+?\\r\\n", nil)
                ]
        -- Diplomats/princesses: remove needless bowing animation (same as deselect animation)
        r2 =    [ ("^anim\\s+selected_to_stand[^\\r]+?Stratmap_Diplomat.+?\\r\\n", nil)
                ]
        r3 =    [ ("^anim\\s+selected_to_stand[^\\r]+?Stratmap_Princess.+?\\r\\n", nil)
                ]

_DSM_FUNCS = addTrueTest [r1, r2]
    where
        -- Reduce "distance to capital" penalty by 75%
        r1 =    [ ("CAPITAL.+?value=\"", id)
                , ("1\\.0", only "0.25")
                ]
        -- Reduce "religious unrest" penalty by 50%
        r2 =    [ ("UNREST.+?value=\"", id)
                , ("1\\.0", only "0.5")
                , (".+?0\\.5.+?", id)
                , (multRoundInt 0.5)
                ]

_DSR_FUNCS = addTrueTest [r1, r2]
    where
        -- Gold resource worth 2
        r1 =    [ ("^type\\s+gold\\r\\ntrade_value\\s+", id)
                , (_REGEX_INT, only "2")
                ]
        -- Silver resource worth 1
        r2 =    [ ("^type\\s+silver\\r\\ntrade_value\\s+", id)
                , (_REGEX_INT, only "1")
                ]

_DW_FUNCS = addTrueTest [r1, r2, r3, r4, r5, r6, r7, r8, rN]
    where
        -- Walls and gate HP 5x
        r1 =    [ ("^\\s+gate\\s.+?full_health\\s", id)
                , (multRoundInt 5)
                ]
        r2 =    [ ("^\\s+wall.+?full_health\\s", id)
                , (multRoundInt 5)
                ]
        r3 =    [ ("^\\s+gateway.+?full_health\\s", id)
                , (multRoundInt 5)
                ]
        -- Tower HP 2x
        r4 =    [ ("^\\s+tower.+?full_health\\s", id)
                , (multRoundInt 2)
                ]
        -- Tower non-flaming firing rate 2x
        r5 =    [ ("^\\s+fire_rate\\s+small\\s+", id)
                , (multRoundInt 0.5)
                ]
        r6 =    [ ("^\\s+fire_rate\\s+normal\\s+", id)
                , (multRoundInt 0.5)
                ]
        r7 =    [ ("^\\s+fire_rate\\s+large\\s+", id)
                , (multRoundInt 0.5)
                ]
        r8 =    [ ("^\\s+fire_rate\\s+huge\\s+", id)
                , (multRoundInt 0.5)
                ]
        -- City/Castle defense tower activation range 8x
        rN =    [ ("^\\s+control_area_radius\\s+", id)
                , (multRoundInt 8)
                ]

_EDCT_FUNCS = addTrueTest [rN]
    where
        -- Remove corruption trigger based on high treasury
        rN =    [ ("^Trigger corruption.+?;-+", nil)
                ]

_EDB_FUNCS = addTrueTest [r1, r2, r3, r4, r5, r6, r7, r8, r9, r3']
    where
        -- Mining income 50x
        r1 =    [ ("^\\s+mine_resource\\s+", id)
                , (multRoundInt 50)
                ]
        -- All building constructions take 1 turn
        r2 =    [ ("^\\s+construction\\s+", id)
                , (_REGEX_INT, only "1")
                ]
        -- Increased building cost. The extra costs are graduated, with the following formula:
        --      NEW = OLD + (((OLD - 400)/400) * 400)
        --
        r3 =    [ ("^\\s+cost\\s+", id)
                , (_REGEX_INT, gradCost gradFormula)
                ]
        gradFormula :: Double -> Int
        gradFormula n = round $ n + (((n - 400)/400) * 400)
        -- Make a note about the changes in the EDB file (comments)
        r3' =   [ ("^;This.+?by hand.+?\\r\\n", prepend $ costsDiffNote "Building cost" old new chg)
                ]
            where
                old =   [ 400
                        , 600
                        , 800
                        , 1000
                        , 1200
                        , 1600
                        , 2000
                        , 2400
                        , 3000
                        , 3200
                        , 3500
                        , 3600
                        , 4800
                        , 6400
                        , 9600
                        , 10000
                        , 12000
                        , 15000
                        ]
                new = applyFormula gradFormula old
                chg = getChgFactors new old
        -- Give free upkeep slots to castles (vanilla cities are 2, 3, 4, 5, 6)
        r4 =    [ ("^\\s{8}motte_and_bailey.+?wall_level.+?", id)
                , (_REGEX_INT, up "1")
                ]
        r5 =    [ ("^\\s{8}wooden_castle.+?wall_level.+?", id)
                , (_REGEX_INT, up "2")
                ]
        r6 =    [ ("^\\s{8}castle.+?wall_level.+?", id)
                , (_REGEX_INT, up "3")
                ]
        r7 =    [ ("^\\s{8}fortress.+?wall_level.+?", id)
                , (_REGEX_INT, up "4")
                ]
        r8 =    [ ("^\\s{8}citadel.+?wall_level.+?", id)
                , (_REGEX_INT, up "5")
                ]
        -- All free upkeep slots 2x
        r9 =    [ ("^\\s+free_upkeep\\s+bonus\\s+", id)
                , (multRoundInt 2)
                ]
        up :: String -> (BC.ByteString -> BC.ByteString)
        up amt = (\d -> BC.append d (BC.pack $ "\r\n" ++ replicate 16 ' ' ++ "free_upkeep bonus " ++ amt))

_EDU_FUNCS = addTrueTest [r1, r2, r3] ++ [rN]
    where
        -- Bodyguard soldiers (cavalry and infantry types) reduced 0.5x; costs reduced accordingly
        -- NOTE: Technically, this regex only catches "real" bodyguards from the campaign game;
        -- hero units in custom battles (King Richard, Duke William, etc.) are also heavy cavalry
        -- general's bodyguard units, but they are left alone.
        r1 =    [ ("^dictionary\\s+\\w+?_Bodyguard.+?soldier\\s+\\w+?_Bodyguard,\\s+", id)
                , (multRoundInt 0.5) -- soldiers 0.5x
                , (".+?stat_cost\\s+\\d+,\\s+", id)
                , (multRoundInt 0.5) -- recruitment cost
                , (",\\s+", id)
                , (multRoundInt 0.5) -- upkeep cost
                , (",\\s+", id)
                , (multRoundInt 0.5) -- weapon upgrade cost
                , (",\\s+", id)
                , (multRoundInt 0.5) -- armor upgrade cost
                , (",\\s+", id)
                , (multRoundInt 0.5) -- custom battle: recruitment cost
                , (",\\s+\\d+,\\s+", id) -- custom battle: recruitment count before penalty (skip)
                , (multRoundInt 0.5) -- custom battle: over-recruitment penalty
                ]
        -- Missile infantry ammo 2x
        r2 =    [ ("^category\\s+infantry\\s+class\\s+missile.+?stat_pri\\s+.+?,.+?,.+?,.+?,\\s+", id)
                , (multRoundInt 2)
                ]
        -- Pikemen units: fix rubber swords bug
        r3 =    [ ("^stat_pri_attr\\s+.+?,\\s+long_pike.+?", id)
                , ("^stat_sec.+?\\r\\n", only "stat_sec         0, 0, no, 0 0, no, melee_simple, blunt, none, 25, 1\r\n")
                ]
        -- Basic infantry have free upkeep (mental up to 5 AND cost up to 650)
        rN =    [ ("^category\\s+infantry.+?", id, alwaysTrue)
                , ("^attributes[^\\r]+", add ", free_upkeep_unit", \s -> not $ BC.isInfixOf (BC.pack "free_upkeep_unit") s)
                , (".+?^stat_mental\\s+", id, alwaysTrue)
                , (_REGEX_INT, id, numTest (<= 5))
                , (".+?^stat_cost\\s+\\d+,\\s+", id, alwaysTrue)
                , (_REGEX_INT, id, numTest (<= 650))
                ]

nil :: BC.ByteString -> BC.ByteString
nil str = BC.empty

only :: String -> BC.ByteString -> BC.ByteString
only repl _ = BC.pack repl

prepend :: String -> BC.ByteString -> BC.ByteString
prepend str orig = BC.append (BC.pack str) orig

add :: String -> BC.ByteString -> BC.ByteString
add str orig = BC.append orig $ BC.pack str

numTest :: (Int -> Bool) -> BC.ByteString -> Bool
numTest f byteStr = case BC.readInt byteStr of
    Just (n, _) -> (f n)
    _ -> False

addTrueTest :: [[(String, (BC.ByteString -> BC.ByteString))]] -> [[(String, (BC.ByteString -> BC.ByteString), (BC.ByteString -> Bool))]]
addTrueTest = map (map (\(a, b) -> (a, b, alwaysTrue)))

alwaysTrue :: BC.ByteString -> Bool
alwaysTrue _ = True

multRoundInt :: Double -> (String, BC.ByteString -> BC.ByteString)
multRoundInt m = (_REGEX_INT, mult m)

multDouble :: Double -> (String, BC.ByteString -> BC.ByteString)
multDouble m = (_REGEX_DOUBLE, mult' m)

-- Produce an integer value.
mult :: Double -> BC.ByteString -> BC.ByteString
mult d n = BC.pack . show . round $ n' * d
    where
        n' :: Double
        n' = if BC.isInfixOf (BC.pack ".") n
                then read (BC.unpack n)::Double
                else case BC.readInt n of
                    Just (n, _) -> fromIntegral n
                    _ -> 0.0

-- Produce a double value, with up to 4 digits after the decimal point.
mult' :: Double -> BC.ByteString -> BC.ByteString
mult' d n = BC.pack . TP.printf "%.4f" $ n' * d
    where
        n' = if BC.isInfixOf (BC.pack ".") n
                then read (BC.unpack n)::Double
                else case BC.readInt n of
                    Just (n, _) -> fromIntegral n
                    _ -> 0.0

applyFormula :: (Double -> Int) -> [Int] -> [Int]
applyFormula f ns = map (f . fromIntegral) ns

gradCost :: (Double -> Int) -> BC.ByteString -> BC.ByteString
gradCost formula i = BC.pack . show . formula $ fromIntegral i'
    where
        i' = case BC.readInt i of
                Just (n, _) -> n
                _ -> 0

getChgFactors :: [Int] -> [Int] -> [Double]
getChgFactors n o = map getChgFactor (zip n o)
    where
        getChgFactor :: (Int, Int) -> Double
        getChgFactor (n, o) = (fromIntegral n)/(fromIntegral o)

-- takes a list of strings and functions, such as [("aaa", foo), ("bbb", bar)] and performs a global
-- substitution on them, as follows:
--      1. create a regex "(aaa)(bbb)"
--      2. replace "aaa"'s match by applying foo on it
--      3. replace "bbb"'s match by applying bar on it
--      4. etc. etc.
grpGsub :: [(String, BC.ByteString -> BC.ByteString, BC.ByteString -> Bool)] -> BC.ByteString -> BC.ByteString
grpGsub grps src =
    sub cnt re funcs src
    where
        parenthesize s = BC.pack $ "(" ++ s ++ ")"
        re = BC.concat $ map (parenthesize . fst3) grps
        funcs = zip3 [1..] (map snd3 grps) (map thd3 grps)
        cnt = length (match re src) - 1

-- Process the results of a matchAllText with an association list of regex replacement functions;
-- replacement occurs only if every subgroup regex's accompanying test function passes. E.g., if
-- funcs is
--     [ (1 , id, alwaysTrue)
--     , (2 , add ", free_upkeep_unit", \s -> not $ BC.isInfixOf (BC.pack "free_upkeep_unit") s)
--     , (3, id, alwaysTrue)
--     , (4, id, numTest (<= 5))
--     , (5, id, alwaysTrue)
--     , (6, id, numTest (<= 650))
--     ]
-- then funcs' is
--     [ (1 , id)
--     , (2 , add ", free_upkeep_unit")
--     , (3,  id)
--     , (4,  id)
--     , (5,  id)
--     , (6,  id)
--     ]
-- and funcs'' is
--     [ (1 , alwaysTrue)
--     , (2 , \s -> not $ BC.isInfixOf (BC.pack "free_upkeep_unit") s)
--     , (3, alwaysTrue)
--     , (4, numTest (<= 5))
--     , (5, alwaysTrue)
--     , (6, numTest (<= 650))
--     ]
-- . The regex looks (essentially) like "(\\1)(\\2)(\\3)(\\4)(\\5)(\\6)" because of grpGsub. The
-- entire regex's successful match (subgroups 1-6) are ONLY processed by the replacement functions
-- in funcs'(subgroups 1-6) if ALL of the tests in funcs'' return True.
sub :: Int -> BC.ByteString -> [(Int, BC.ByteString -> BC.ByteString, BC.ByteString -> Bool)] -> BC.ByteString -> BC.ByteString
sub (-1) _ _ src = src
sub cnt re funcs src =
    sub (cnt - 1) re funcs maybeSub
    where
        minfos = match re src
        minfo = minfos!!cnt
        funcs' = zip (map fst3 funcs) (map snd3 funcs)
        funcs'' = zip (map fst3 funcs) (map thd3 funcs)
        maybeSub = if testsOK
            then sub' minfo funcs' src
            else src
        testsOK = all (== True) tests
            where
                kvs = assocs minfo
                tests = map test kvs
                test (k, v@(str, (_, _))) = case lookup k funcs'' of
                    Just func -> func str
                    _ -> True -- since minfo includes the entire regex's full match info as the first (k, v), we will always visit this codepath (since we zip up the replacment functions starting from k = 1, and the full match info's k is 0), so we need to let the k = 0 case succeed.

-- manually replaces text using MatchText info, but intelligently with an association list of string
-- manipulation functions (where key corresponds to the group that this function will act on)
sub' :: MatchText BC.ByteString -> [(Int, BC.ByteString -> BC.ByteString)] -> BC.ByteString -> BC.ByteString
sub' matchInfo assocFuncs src =
    BC.append (BC.append (BC.take (fromIntegral pos) src) replacement) (BC.drop (fromIntegral (pos + bytes)) src)
    where
        kvs = assocs matchInfo
        (fullMatchStr, (pos, bytes)) = matchInfo!0
        -- We use foldl' here to incrementally construct the full replacement bytestring; if we were
        -- to use BC.concat $ map ..., then the thunk size will grow (possibly very large) depending
        -- on the number of regex groups.
        replacement = foldl' BC.append BC.empty $ map replaceGrps kvs
        replaceGrps (k, v@(str, (_, _))) = case lookup k assocFuncs of
            Just func -> func str
            _ -> BC.empty -- we will always visit this codepath because k = 0 exists for the fullMatchStr case; since we only do replacments on a per-group (k = 1, k = 2, etc.) basis, we skip the fullMatchStr case by making it an empty string

match :: BC.ByteString -> BC.ByteString -> [MatchText BC.ByteString]
match re = matchAllText (makeRegexDef re)

splitBy :: Regex -> BC.ByteString -> [BC.ByteString]
splitBy delim strIn
    | BC.null strIn = []
    | otherwise =
        let matches = map (!0) (matchAll delim strIn)
            go _ str [] = str : []
            go i str ((off,len):rest) =
                let i' = off+len
                    firstline = BC.take (off-i) str
                    remainder = BC.drop (i'-i) str
                in seq i' $
                    if BC.null remainder
                        then [firstline, BC.empty]
                        else firstline : go i' remainder rest
        in go 0 strIn matches

makeRegexDef :: BC.ByteString -> Regex
makeRegexDef re = makeRegexOpts copts eopts re
    where
        copts = sum [compDotAll, compMultiline]
        eopts = sum [execBlank]
