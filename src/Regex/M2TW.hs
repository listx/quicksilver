module Regex.M2TW where

import qualified Data.ByteString.Char8 as BC

import Regex
import Data.Recruit.M2TW
import Util

_M2TW_DC_FUNCS :: RegexSets
_M2TW_DC_FUNCS = addTrueTest [r1, r2, r3] ++ addTrueTest assassin
    where
        -- Ship movement speed 3x
        r1 =    [ ("^type\\s+admiral.+?starting_action_points\\s+", id)
                , (multRoundInt 3)
                ]
        -- Diplomat movement speed 2x
        r2 =    [ ("^type\\s+diplomat.+?starting_action_points\\s+", id)
                , (multRoundInt 2)
                ]
        -- Campaign movement speed 1.75x
        r3 =    [ ("^starting_action_points\\s+", id)
                , (multRoundInt 1.75)
                ]
        assassin =
            [
                -- Assassin upkeep cost 3x
                [ ("^type\\s+assassin\\s+[\\w\\s,]+?\\r\\nwage_base\\s+", id)
                , (multRoundInt 3)
                ]
            ]

_M2TW_DCD_FUNCS :: RegexSets
_M2TW_DCD_FUNCS = addTrueTest [agent, rebels] ++ addTrueTest (witchesHereticsInquisitors ++ assassin)
    where
        -- Allow a settlement to recruit 2 agents of the same type in a single
        -- turn.
        agent =
                [ ("max_agents_per_turn.+?\"", id)
                , (multRoundInt 2)
                ]
        -- Remove witches, heretics, and inquisitors.
        witchesHereticsInquisitors =
            [
                [ ("max_witches uint.+?\"", id)
                , (_REGEX_INT, only "0")
                ]
            ,
                [ ("max_witches_per_region uint.+?\"", id)
                , (_REGEX_INT, only "0")
                ]
            ,
                [ ("witch_creation.+?\"", id)
                , (_REGEX_DOUBLE, only "0.0")
                ]
            ,
                [ ("max_heretics uint.+?\"", id)
                , (_REGEX_INT, only "0")
                ]
            ,
                [ ("max_heretics_per_region uint.+?\"", id)
                , (_REGEX_INT, only "0")
                ]
            ,
                [ ("heretic_creation.+?\"", id)
                , (_REGEX_DOUBLE, only "0.0")
                ]
            ,
                [ ("max_inquisitors uint.+?\"", id)
                , (_REGEX_INT, only "0")
                ]
            ,
                [ ("max_inquisitors_per_region uint.+?\"", id)
                , (_REGEX_INT, only "0")
                ]
            ,
                [ ("inquisitor_creation.+?\"", id)
                , (_REGEX_DOUBLE, only "0.0")
                ]
            ]
        assassin =
            [
                -- Improve effectiveness of assassins 2x
                [ ("assassinate_base_chance.+?\"", id)
                , (multRoundInt 2)
                ]
            ,
                -- Assassin's minimum success chance changed from 5% to 17% (1
                -- out of 6, just like rolling a die; so a newbie assassin
                -- should be able to succeed more easily), and maximum chance
                -- changed from 95% to 99%
                [ ("assassinate_chance_min.+?\"", id)
                , (_REGEX_INT, only "17")
                ]
            ,
                [ ("assassinate_chance_max.+?\"", id)
                , (_REGEX_INT, only "99")
                ]
            ]
        rebels =
                [ ("min_turn_keep_rebel_garrison.+?\"", id)
                , (_REGEX_INT, only "0")
                ]

_M2TW_DCAD_FUNCS :: RegexSets
_M2TW_DCAD_FUNCS = addTrueTest [r1]
    where
        -- Stop Papal States from capturing rebel settlements (helps
        -- land-strapped factions like Milan and Sicily expand into Africa more
        -- easily)
        r1 =    [ ("papal_faction.+?invasion_decisions.+?", id)
                , ("<decision_entry>.+?", id)
                , ("<decision_entry>.+?turn_number=\"", id)
                , (_REGEX_INT, only "225")
                , (".+?<decision_entry>.+?", id)
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

_M2TW_DCL_FUNCS :: RegexSets
_M2TW_DCL_FUNCS = addTrueTest [rN]
    where
        -- Assassin recruitment cost 3x
        rN =    [ ("^assassin.+?assassin\\.tga\\s+", id)
                , (multRoundInt 3)
                ]

_M2TW_DFS_FUNCS :: RegexSets
_M2TW_DFS_FUNCS = addTrueTest [rN]
    where
        -- Fixed faction standing bug
        rN =    [ ("^;Trigger 0102_city_razed.+?;-+", nil)
                ]

_M2TW_DM_FUNCS :: RegexSets
_M2TW_DM_FUNCS = addTrueTest [r1, r2, r3, r4a, r4b, r5, r6a, r6b, r4'] ++ addTrueTest (merchantsGuild ++ thievesGuild)
    where
        -- Disable all mission penalties, and disable the "cease hostilities"
        -- mission
        r1 =    [ ("^\\s+penalty\\r\\n.+?\\}\\r\\n", nil)
                ]
        r2 =    [ ("^mission\\s+cease.+?^\\}\\r\\n\\r\\n\\r\\n", nil)
                ]
        -- Reduce assassins' guild payback from 10 to 5 guild points.
        r3 =    [ ("guild_money\\s+", id)
                , (_REGEX_INT, only "5")
                , ("\\s+assassins_guild", id)
                ]
        -- Increase all missions' monetary rewards with this formula:
        --      NEW = OLD + (((OLD - 100)/100) * 2000)
        --
        r4a =   [ ("^\\s+money\\s+", id)
                , (_REGEX_INT, gradCost moneyFormula)
                ]
        -- Fix council_min/mod_money bug (the lines starting with "    cash ..."
        -- were not correctly mapped to their intended paybacks)
        --
        -- Also, increase the cash threshold 10x for triggering the
        -- major/mod/min money rewards
        r4b =   [ ("^\\s+cash\\s+", id)
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
        -- Decrease mission durations by 33% (except for the "convert" mission
        -- where you have to convert a settlement's religion)
        r6a =   [ ("\\s+duration\\s+", id)
                , (multRoundInt 0.66)
                ]
        -- Change the 'convert' mission's duration back to 15
        r6b =   [ ("^mission convert.+?duration\\s+", id)
                , (_REGEX_INT, only "15")
                ]
        -- Disable merchant recruitment/acquisition missions.
        merchantsGuild =
            [
                [ ("^mission guild_recruit_agent guild_recruit_merchant.+?\\}.+?\\}\\r\\n", nil)
                ]
            ,
                [ ("^mission guild_acquisition.+?\\}.+?\\}\\r\\n", nil)
                ]
            ]
        -- Disable the thief recruitment mission (the only thieves' guild
        -- mission, coincidentally).
        thievesGuild =
            [
                [ ("^mission guild_recruit_agent guild_recruit_spy.+?\\}.+?\\}\\r\\n", nil)
                ]
            ]

_M2TW_DS_FUNCS :: RegexSets
_M2TW_DS_FUNCS = addTrueTest [r1, r2, r3]
    ++ [r4, r5, r6, r7]
    ++ addTrueTest (noMerchantPrincessSpy ++ mtePurse)
    ++ mineFuncs capsSecs
    ++ mineFuncs capsSecs'
    ++ addTrueTest durazzo
    ++ addTrueTest (noFarmsTaverns ++ giveRoads ++ rebels)
    where
        -- Rebel spawn rate 20x lower
        r1 =    [ ("^brigand_spawn_value\\s+", id)
                , (multRoundInt 20)
                ]
        -- Pirate spawn rate 20x lower
        r2 =    [ ("^pirate_spawn_value\\s+", id)
                , (multRoundInt 20)
                ]
        -- King's purse 2x
        r3 =    [ ("^denari_kings_purse\\s+", id)
                , (multRoundInt 2)
                ]
        -- Replace all gold resources with chocolate in the map, except for
        -- Aztecs
        r4 =    [ ("^resource\\s+", id, alwaysTrue)
                , ("gold", only "chocolate", alwaysTrue)
                , (",\\s+", id, alwaysTrue)
                , (_REGEX_INT, id, numTest (>= 30))
                , (",\\s+\\d+\\r\\n", id, alwaysTrue)
                ]
        -- Replace all silver resources with silk in the the map, except for
        -- Aztecs
        r5 =    [ ("^resource\\s+", id, alwaysTrue)
                , ("silver", only "silk", alwaysTrue)
                , (",\\s+", id, alwaysTrue)
                , (_REGEX_INT, id, numTest (>= 30))
                , (",\\s+\\d+\\r\\n", id, alwaysTrue)
                ]
        -- Add in 1 gold resource for every faction's capital city region
        r6 =    [ ("^resource\\s+gold,\\s+5,\\s+143\\r\\n", addResource "gold" goldCoords, alwaysTrue)
                ]
        -- Add in 1 silver resource for every faction's second city (or the
        -- capital city if no second city)
        r7 =    [ ("^resource\\s+silver,\\s+4,\\s+123\\r\\n", addResource "silver" silverCoords, alwaysTrue)
                ]
        -- Remove all merchants, princesses, and spies from the starting map.
        noMerchantPrincessSpy =
            [
                [ ("character[^\\r]+merchant.+?\\r\\n.+?\\r\\n", nil)
                ]
            ,
                [ ("character[^\\r]+princess.+?\\r\\n.+?\\r\\n", nil)
                ]
            ,
                [ ("character[^\\r]+spy.+?\\r\\n.+?\\r\\n", nil)
                ]
            ,
                [ ("^ancillaries catamite\\r\\n", nil)
                ]
            ,
                [ ("^ancillaries spymaster\\r\\n", nil)
                ]
            ,
                -- Remove all references to princess names: Cecilia, Constance,
                -- Agnes, Urraca, Teresa, Matilda, Anna, Antonina, Ingrid,
                -- Maria, Agnes, Pioska
                [ ("^relative\\s+William.+?", id)
                , ("Cecilia,", nil)
                ]
            ,
                [ ("^relative\\s+Philip.+?", id)
                , ("Constance,", nil)
                ]
            ,
                [ ("^relative\\s+Heinrich.+?", id)
                , ("Agnes,", nil)
                ]
            ,
                [ ("^relative\\s+Alfonso.+?", id)
                , ("Urraca,\\s+Teresa,", nil)
                ]
            ,
                [ ("^relative\\s+Roger.+?", id)
                , ("Matilda,", nil)
                ]
            ,
                [ ("^relative\\s+Alexius.+?", id)
                , ("Anna,", nil)
                ]
            ,
                [ ("^relative\\s+Ysevolod.+?", id)
                , ("Antonina,", nil)
                ]
            ,
                [ ("^relative\\s+Knud.+?", id)
                , ("Ingrid,", nil)
                ]
            ,
                [ ("^relative\\s+Henrique.+?", id)
                , ("Maria,", nil)
                ]
            ,
                [ ("^relative\\s+Wladyslaw.+?", id)
                , ("Agnes,", nil)
                ]
            ,
                [ ("^relative\\s+Laszlo.+?", id)
                , ("Pioska,", nil)
                ]
            ]
        showResource :: String -> (Int, Int) -> String
        showResource res (x, y) = "resource\t" ++ res ++ ",\t" ++ show x ++ ",\t" ++ show y ++ "\r\n"
        addResource :: String -> [(Int, Int)] -> BC.ByteString -> BC.ByteString
        addResource res coords s = BC.append s $ BC.pack (concatMap (showResource res) coords)
        -- Gold resource locations, one for each faction's capital (except
        -- Aztecs)
        goldCoords =
            [ (97,174) -- Scotland
            , (93, 145) -- England
            , (114, 120) -- France
            , (66, 105) -- Spain
            , (57, 81) -- Portugal
            , (69, 74) -- Moors
            , (130, 108) -- Milan
            , (151, 66) -- Sicily
            , (148, 93) -- Papal States (Rome region)
            , (145, 91) -- Papal States (Rome region)
            , (147, 88) -- Papal States (Rome region)
            --, (147, 94) -- Papal States (Rome region)
            --, (149, 86) -- Papal States (Rome region)
            --, (144, 92) -- Papal States (Rome region)
            --, (149, 92) -- Papal States (Rome region)
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
            [ (99, 173) -- Scotland
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
        -- Give Moors, Turks, and Egypt extra king's purse --- 1.5x, 1.5x, and
        -- 1.75x, respectively.
        mtePurse =
            [
                [ ("^faction\\smoors.+?denari_kings_purse\\s+", id)
                , (multRoundInt 1.5)
                ]
            ,
                [ ("^faction\\sturks.+?denari_kings_purse\\s+", id)
                , (multRoundInt 1.5)
                ]
            ,
                [ ("^faction\\segypt.+?denari_kings_purse\\s+", id)
                , (multRoundInt 1.75)
                ]
            ]
        -- Give mines to all factions' capitals/secondary cities, because the AI
        -- is too stupid and doesn't always build mines immediately.
        mine =
            "\r\n\tbuilding\r\n\
            \\t{\r\n\
            \\t\ttype hinterland_mines mines\r\n\
            \\t}"
        c_mine =
            "\r\n\tbuilding\r\n\
            \\t{\r\n\
            \\t\ttype hinterland_castle_mines c_mines\r\n\
            \\t}"
        mineFuncs :: (Bool, [String]) -> RegexSets
        mineFuncs (b, strs) = concatMap makeFunc strs
            where
                makeFunc str = addTrueTest
                    [
                        [ (str ++ "_.+?", id)
                        , ("\\r\\n\\}", prepend chooseMine)
                        ]
                    ]
                chooseMine = if b
                    then mine
                    else c_mine
        capsSecs = -- cities
            ( True,
            [ "Cordoba"
            , "Lisbon"
            , "Leon"
            , "Paris"
            , "Rheims"
            , "London"
            , "Edinburgh"
            , "Arhus"
            , "Frankfurt"
            , "Nuremburg"
            , "Milan"
            , "Genoa"
            , "Venice"
            , "Roman" -- "Rome" in-game
            , "Naples"
            , "Novgorod"
            , "Cracow" -- "Krakow" in-game
            , "Budapest"
            , "Constantinople"
            , "Nicaea"
            , "Iconium"
            , "Cairo"
            ]
            )
        capsSecs' = -- castles
            ( False,
            [ "Granada"
            , "Pamplona"
            , "Toledo"
            , "Nottingham"
            , "Ragusa"
            , "Palermo"
            , "Halych"
            , "Bran"
            , "Caesarea"
            , "Gaza"
            ]
            )
        durazzo =
            [
                [ ("Smyrna_Province.+?}.+?}\\r\\n", add entry)
                ]
            ]
            where
                entry = "settlement\r\n{\r\n"
                    ++ "\tlevel village\r\n"
                    ++ "\tregion Durazzo_Province\r\n\r\n"
                    ++ "\tyear_founded 0\r\n"
                    ++ "\tpopulation 800\r\n"
                    ++ "\tplan_set default_set\r\n"
                    ++ "\tfaction_creator byzantium\r\n"
                    ++ "}\r\n"
        -- Give all settlements paved roads.
        roads =
            "\r\n\tbuilding\r\n\
            \\t{\r\n\
            \\t\ttype hinterland_roads paved_roads\r\n\
            \\t}"
        roadsCastle =
            "\r\n\tbuilding\r\n\
            \\t{\r\n\
            \\t\ttype hinterland_castle_roads c_paved_roads\r\n\
            \\t}"
        giveRoads =
            [
                -- first, remove all road buildings
                [ ("^\\s+building\\r\\n\\s+\\{\\r\\n\\s+type\\s+hinterland_roads\\s+roads\\r\\n\\s+\\}\\r\\n", nil)
                ]
            ,
                [ ("^\\s+building\\r\\n\\s+\\{\\r\\n\\s+type\\s+hinterland_castle_roads\\s+c_roads\\r\\n\\s+\\}\\r\\n", nil)
                ]
            ,
                -- now add paved_roads for settlements and c_paved_roads for
                -- castles
                [ ("^settlement\\r\\n.+?", id)
                , ("\\r\\n\\}", prepend roads)
                ]
            ,
                [ ("^settlement\\s+castle\\r\\n.+?", id)
                , ("\\r\\n\\}", prepend roadsCastle)
                ]
            ]
        noFarmsTaverns =
            [
                [ ("^\\s+building\\r\\n\\s+\\{\\r\\n\\s+type\\s+hinterland_farms\\s+farms\\r\\n\\s+\\}\\r\\n", nil)
                ]
            ,
                [ ("^\\s+building\\r\\n\\s+\\{\\r\\n\\s+type\\s+taverns.+?\\r\\n\\s+\\}\\r\\n", nil)
                ]
            ]
        rebels =
            [
                -- give rebels 5,000,000 gold to start with
                [ ("slave.+?denari.+?", id)
                , (_REGEX_INT, only "5000000")
                ]
            ,
                -- give rebels 500,000 gold per turn
                [ ("slave.+?denari_kings_purse.+?", id)
                , (_REGEX_INT, only "500000")
                ]
            ,
                -- change AI from rebels to default (just like all the
                -- non-catholic factions)
                [ ("slave_faction", only "default")
                ]
            ]

_M2TW_DSF_FUNCS :: RegexSets
_M2TW_DSF_FUNCS = addTrueTest [r1]
    where
        -- Prevent all factions from getting princesses
        r1 =    [ ("^can_have_princess[^\\r]+?", id)
                , ("yes", only "no")
                ]

-- For the DSK file to be parsed and new packed animation files
-- ({pack,skeletons}.{dat,idx}) to be generated inside the data/animations
-- directory by the medieval2.exe binary, the game needs access to the unpacked
-- animation files (released semi-officially by `Caliban' (of twcenter.net
-- forums and an ex? employee of Creative Assembly). First, copy over the
-- unpacked animation files into the `animations' folder. The exe will then
-- generate new packed animation files and store them under the animations
-- directory. If we've launched the exe referencing our modfolder, the new
-- packed files will then be inside the animations folder inside our modfolder
-- (otherwise, the exe will probably overwrite the old, official packed files).
-- Then, it's simply a matter of doing a binary diff of the modfolder's packed
-- animation files versus the old official packed files to generate a bdiff,
-- which can then be added to our version control for sensible tracking and
-- quick generation of new packed animation files on subsequent (non-animation)
-- modifications.
_M2TW_DSK_FUNCS :: RegexSets
_M2TW_DSK_FUNCS = addTrueTest [r1, r2, r3]
    where
        -- Diplomats: remove annoying "conduct diplomacy" animation
        r1 =    [ ("^anim\\s+conduct_diplomacy.+?\\r\\n", nil)
                ]
        -- Diplomats: remove needless bowing animation (same as deselect animation)
        r2 =    [ ("^anim\\s+selected_to_stand[^\\r]+?Stratmap_Diplomat.+?\\r\\n", nil)
                ]
        -- Princesses: remove needless bowing animation
        r3 =    [ ("^anim\\s+selected_to_stand[^\\r]+?Stratmap_Princess.+?\\r\\n", nil)
                ]


_M2TW_DSM_FUNCS :: RegexSets
_M2TW_DSM_FUNCS = addTrueTest [r1, r2, r3, r4]
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
        -- Reduce population requirements to upgrade a settlement, such that smaller settlements can
        -- get upgraded more quickly. This will help prevent settlements from stagnating as villages
        -- and towns (or mottes/baileys). The smaller populations get more population upgrade
        -- reductions than the bigger ones.
        r3 =    [ ("level.+?village.+?upgrade=\"", id)
                , (multRoundInt 0.6)
                , (".+?level.+?town.+?base=\"", id)
                , (multRoundInt 0.6)
                , (".+?upgrade=\"", id)
                , (multRoundInt 0.6)
                , (".+?level.+?large_town.+?base=\"", id)
                , (multRoundInt 0.6)
                , (".+?upgrade=\"", id)
                , (multRoundInt 0.7)
                , (".+?level.+?city.+?base=\"", id)
                , (multRoundInt 0.7)
                , (".+?upgrade=\"", id)
                , (multRoundInt 0.8)
                , (".+?level.+?large_city.+?base=\"", id)
                , (multRoundInt 0.8)
                , (".+?upgrade=\"", id)
                , (multRoundInt 0.9)
                , (".+?level.+?huge_city.+?base=\"", id)
                , (multRoundInt 0.9)
                ]
        r4 =    [ ("level[^\\r]+?\"castle.+?base=\"", id)
                , (multRoundInt 0.7)
                , (".+?upgrade=\"", id)
                , (multRoundInt 0.7)
                , (".+?level.+?fortress.+?base=\"", id)
                , (multRoundInt 0.8)
                , (".+?upgrade=\"", id)
                , (multRoundInt 0.8)
                , (".+?level.+?citadel.+?base=\"", id)
                , (multRoundInt 0.9)
                , (".+?upgrade=\"", id)
                , (multRoundInt 0.9)
                , (".+?max=\"", id)
                , (multRoundInt 0.9)
                ]

_M2TW_DSR_FUNCS :: RegexSets
_M2TW_DSR_FUNCS = addTrueTest [r1, r2, r3]
    where
        -- Gold resource worth 2
        r1 =    [ ("^type\\s+gold\\r\\ntrade_value\\s+", id)
                , (_REGEX_INT, only "2")
                ]
        -- Silver resource worth 1
        r2 =    [ ("^type\\s+silver\\r\\ntrade_value\\s+", id)
                , (_REGEX_INT, only "1")
                ]
        -- Only allow gold and silver to be minable.
        r3 =    [ ("^has_mine.+?", id)
                , ("^has_mine.+?", id)
                , ("\\r\\nhas_mine", nil)
                , (".+?", id)
                , ("\\r\\nhas_mine", nil)
                , (".+?", id)
                , ("\\r\\nhas_mine", nil)
                , (".+?", id)
                , ("\\r\\nhas_mine", nil)
                , (".+?", id)
                , ("\\r\\nhas_mine", nil)
                ]

_M2TW_DW_FUNCS :: RegexSets
_M2TW_DW_FUNCS = addTrueTest [r1, r2, r3, r4, r5, r6, r7, r8, rN]
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
        -- Tower non-flaming firing rate 2x (i.e., reduce firing delay by 0.5x)
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

_M2TW_EDCT_FUNCS :: RegexSets
_M2TW_EDCT_FUNCS = addTrueTest [r1, r2, r3]
    where
        -- Remove corruption trigger based on high treasury
        r1 =    [ ("^Trigger corruption.+?;-+", nil)
                ]
        -- Give good assassins a line of sight bonus with increased skill.
        r2 =    [ ("^Trait GoodAssassin.+?", id)
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
        -- Remove all references to thieves guild.
        r3 =    [ ("^Trigger spyinit4.+?;-+\\r\\n", nil)
                , ("^Trigger spyinit5.+?;-+\\r\\n", nil)
                , ("^Trigger spyinit6.+?;-+\\r\\n", nil)
                ]

_M2TW_EDA_FUNCS :: RegexSets
_M2TW_EDA_FUNCS = addTrueTest [r1, r2] ++ addTrueTest farmsTavernsWharves
    where
        -- Remove advice for mines+1 and c_mines+1 (since they are removed from EDB)
        r1 =    [ ("^Trigger 1062.+?;-+\r\n", nil)
                ]
        r2 =    [ ("^Trigger 1064.+?;-+\r\n", nil)
                ]
        -- Remove advice for farms and taverns.
        farmsTavernsWharves =
            [
                -- farms
                [ ("^Trigger 0734.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1053.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1054.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1055.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1056.+?;-+\r\n", nil)
                ]
            ,
                -- taverns
                [ ("^AdviceThread Construction_Taverns_Advice_Thread.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1113.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1114.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1115.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1116.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1117.+?;-+\r\n", nil)
                ]
            ,
                -- wharves
                [ ("^AdviceThread Construction_Sea_Trade_Advice_Thread.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1110.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1111.+?;-+\r\n", nil)
                ]
            ,
                [ ("^Trigger 1112.+?;-+\r\n", nil)
                ]
            ]

_M2TW_EDAN_FUNCS :: RegexSets
_M2TW_EDAN_FUNCS = addTrueTest [r1]
    where
        -- Remove reference to thieves guild.
        r1 =    [ ("^Trigger spymaster_vnv_trigger2.+?;-+\r\n", nil)
                ]

_M2TW_EDB_FUNCS :: RegexSets
_M2TW_EDB_FUNCS = addTrueTest $ [r1, r2, r3, r3', r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, noThievesGuild]
    ++ noMerchantSpy ++ mergeFarmsTavernsWharves ++ fastPorts ++ recruitment
    where
        -- Mining income 75x (income from 1 mine = mine_resource * 20 * (trade value of resource))
        r1 =    [ ("^\\s+mine_resource\\s+", id)
                , (multRoundInt 75)
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
                , (_REGEX_INT, upkeep "1")
                ]
        r5 =    [ ("^\\s{8}wooden_castle.+?wall_level.+?", id)
                , (_REGEX_INT, upkeep "2")
                ]
        r6 =    [ ("^\\s{8}castle.+?wall_level.+?", id)
                , (_REGEX_INT, upkeep "3")
                ]
        r7 =    [ ("^\\s{8}fortress.+?wall_level.+?", id)
                , (_REGEX_INT, upkeep "4")
                ]
        r8 =    [ ("^\\s{8}citadel.+?wall_level.+?", id)
                , (_REGEX_INT, upkeep "5")
                ]
        -- All free upkeep slots 2x
        r9 =    [ ("^\\s+free_upkeep\\s+bonus\\s+", id)
                , (multRoundInt 2)
                ]
        upkeep :: String -> (BC.ByteString -> BC.ByteString)
        upkeep amt = (\d -> BC.append d (BC.pack $ "\r\n" ++ replicate 16 ' ' ++ "free_upkeep bonus " ++ amt))
        -- Disable the "Mining network" building.
        r10 =   [ ("^building\\s+hinterland_mines.+?levels\\s+mines", id)
                , ("\\s+mines\\+1[^\\r]+", nil)
                , (".+?\\r\\n        {\\r\\n", id)
                , ("\\s+convert_to.+?\\r\\n", nil)
                , (".+?upgrades.+?\\{\\r\\n", id)
                , ("\\s+mines\\+1\\r\\n", nil)
                , (".+?\\}.+?\\}\\r\\n", id)
                , ("^\\s+mines\\+1.+?\\}.+?\\}.+?\\}.+?\\}\\r\\n", nil)
                ]
        r11 =   [ ("^building\\s+hinterland_castle_mines.+?levels\\s+c_mines", id)
                , ("\\s+c_mines\\+1[^\\r]+", nil)
                , (".+?\\r\\n        {\\r\\n", id)
                , ("\\s+convert_to.+?\\r\\n", nil)
                , (".+?upgrades.+?\\{\\r\\n", id)
                , ("\\s+c_mines\\+1\\r\\n", nil)
                , (".+?\\}.+?\\}\\r\\n", id)
                , ("^\\s+c_mines\\+1.+?\\}.+?\\}.+?\\}.+?\\}\\r\\n", nil)
                ]
        -- Only allow mines for gold and silver resources
        r12 =   [ ("^\\s+mines\\s+city\\s+requires\\s+factions.+?resource\\s+gold", id)
                , ("[^\\r]+", nil)
                ]
        r13 =   [ ("^\\s+c_mines\\s+castle\\s+requires\\s+factions.+?resource\\s+gold", id)
                , ("[^\\r]+", nil)
                ]
        -- Increase recruitment slots from
        --     city:   1, 2, 2, 3, 3
        --     castle: 1, 2, 3, 3, 3
        -- to
        --     city:   1, 2, 3, 4, 5
        --     castle: 2, 3, 4, 5, 6
        r14 =   [ ("^building\\s+core_building.+?", id)
                , ("recruitment_slots.+?recruitment_slots.+?recruitment_slots\\s+", id)
                , (_REGEX_INT, only "3")
                , (".+?recruitment_slots\\s+", id)
                , (_REGEX_INT, only "4")
                , (".+?recruitment_slots\\s+", id)
                , (_REGEX_INT, only "5")
                ]
        r15 =   [ ("^building\\s+core_castle_building.+?", id)
                , ("recruitment_slots\\s+", id)
                , (_REGEX_INT, only "2")
                , (".+?recruitment_slots\\s+", id)
                , (_REGEX_INT, only "3")
                , (".+?recruitment_slots\\s+", id)
                , (_REGEX_INT, only "4")
                , (".+?recruitment_slots\\s+", id)
                , (_REGEX_INT, only "5")
                , (".+?recruitment_slots\\s+", id)
                , (_REGEX_INT, only "6")
                ]
        -- Bugfix: give swordsmiths_guild HQ heavy_cavalry_bonus bonus 2, instead of 1 (same as
        -- master swordsmiths guild)
        r16 =   [ ("^building\\s+guild_swordsmiths_guild.+?heavy_cavalry_bonus.+?heavy_cavalry_bonus\\s+bonus\\s+", id)
                , (_REGEX_INT, only "2")
                ]
        -- Disable recruitment of merchants and spies.
        noMerchantSpy =
            [
                [ ("^\\s+agent merchant.+?\\r\\n", nil)
                ]
            ,
                [ ("^\\s+agent spy.+?\\r\\n", nil)
                ]
            ]
        -- Disable thieves' guild building.
        noThievesGuild =
                [ ("^building guild_thiefs_guild.+", only ";")
                ]
        happiness n = replicate 16 ' ' ++ "happiness_bonus bonus " ++ n ++ "\r\n"
        assassin = replicate 16 ' ' ++ "agent assassin  0  requires factions { northern_european, middle_eastern, eastern_european, greek, southern_european, } \r\n"
        tradeFleet n = replicate 16 ' ' ++ "trade_fleet " ++ n ++ "\r\n"
        tradeBaseIncomeBonus n = replicate 16 ' ' ++ "trade_base_income_bonus bonus " ++ n ++ "\r\n"
        mergeFarmsTavernsWharves =
                [
                    -- merge farms into city and castle walls (capability farming_level 1, 2, 3, 4),
                    -- and remove farms
                    [ ("^building\\s+core_building.+?", id)
                    , ("^\\s+wall_level 0\\r\\n", add "                farming_level 1\r\n")
                    , (".+?wall_level 1\\r\\n", add "                farming_level 2\r\n")
                    , (".+?wall_level 2\\r\\n", add "                farming_level 3\r\n")
                    , (".+?wall_level 3\\r\\n", add "                farming_level 4\r\n")
                    ]
                ,
                    [ ("^building\\s+core_castle_building.+?", id)
                    , ("^\\s+wall_level 0\\r\\n", add "                farming_level 1\r\n")
                    , (".+?wall_level 1\\r\\n", add "                farming_level 2\r\n")
                    , (".+?wall_level 2\\r\\n", add "                farming_level 3\r\n")
                    , (".+?wall_level 3\\r\\n", add "                farming_level 4\r\n")
                    ]
                ,
                    -- remove farms
                    [ ("^building hinterland_farms.+", only ";")
                    ]
                  -- merge taverns into markets (transilvanian peasant recruitment is added by the (recruitment) function below)
                ,
                    [ ("^building\\s+market.+?", id)
                    , ("^\\s+agent_limit merchant 1\\r\\n", add $ assassin ++ happiness "1")
                    , (".+?agent_limit merchant 1\\r\\n", add $ assassin ++ happiness "2")
                    , (".+?agent_limit merchant 1\\r\\n", add $ assassin ++ happiness "3")
                    , (".+?agent_limit merchant 1\\r\\n", add $ assassin ++ happiness "4")
                    , (".+?agent_limit merchant 1\\r\\n", add $ assassin ++ happiness "5")
                    ]
                ,
                    -- remove taverns
                    [ ("^building taverns.+", only ";")
                    ]
                ,
                    -- merge merchant's wharf into ports (only city ports, NOT castle ports!)
                    [ ("^building\\s+port.+?capability.+?capability.+?", id)
                    , ("\\{\\r\\n", add $ tradeFleet "1" ++ tradeBaseIncomeBonus "2")
                    , (".+?capability.+?\\{\\r\\n", add $ tradeFleet "2" ++ tradeBaseIncomeBonus "3")
                    , (".+?capability.+?\\{\\r\\n", add $ tradeFleet "3" ++ tradeBaseIncomeBonus "4")
                    ]
                ,
                    -- remove merchant wharves
                    [ ("^building sea_trade.+", only ";")
                    ]
                ]
        fastPorts =
                [
                    -- remove gunpowder and world_is_round requirements for advanced port buildings
                    -- (city and castle)
                    [ ("^\\s+dockyard city.+?\\}", id)
                    , (".+?", nil)
                    , ("\\r", id)
                    ]
                ,
                    [ ("^\\s+naval_drydock city.+?\\}", id)
                    , (".+?", nil)
                    , ("\\r", id)
                    ]
                ,
                    [ ("^\\s+c_dockyard castle.+?\\}", id)
                    , (".+?", nil)
                    , ("\\r", id)
                    ]
                ,
                    [ ("^\\s+c_naval_drydock castle.+?\\}", id)
                    , (".+?", nil)
                    , ("\\r", id)
                    ]
                ]
        -- For every type of building (barracks, archery range, city hall, etc.), let all levels of
        -- that building be able to recruit all units; no more waiting idly until your city becomes
        -- a Huge City to be able to recruit elite units.
        recruitmentsBasic = map makeRegexSnippets . zip (repeat "\\r\\n.+?            \\{\\r\\n") $ concat
            [ zip3 (repeat Core) [[village, town, largeTown, city, largeCity]] (repeat "^\\s+wall_level")
            , zip3 (repeat Castle) [[village, town, largeTown, city, largeCity]] (repeat "^\\s+wall_level")
            , zip3 (repeat Equestrian) [[village, town, largeTown, city, largeCity]] (repeat "^\\s+}")
            , zip3 (repeat Barracks) [[village, town, largeTown, city, largeCity, hugeCity1, hugeCity2]] (repeat "^\\s+}")
            , zip3 (repeat CastleBarracks) [[village, town, largeTown, city, largeCity]] (repeat "^\\s+}")
            , zip3 (repeat ProfessionalMilitary) [[hugeCity1, hugeCity2]] (repeat "^\\s+}")
            , zip3 (repeat Missiles) [[town, largeTown, city, largeCity]] (repeat "^\\s+}")
            , zip3 (repeat Siege) [[largeTown, city, largeCity]] (repeat "^\\s+}")
            , zip3 (repeat CastleSiege) [[largeTown, city, largeCity]] (repeat "^\\s+}")
            , zip3 (repeat Cannon) [[city, largeCity, hugeCity1, hugeCity2]] (repeat "^\\s+}")
            , zip3 (repeat CastleCannon) [[city, largeCity, hugeCity1, hugeCity2]] (repeat "^\\s+}")
            , zip3 (repeat UrbanEquestrian) [[city, largeCity]] (repeat "^\\s+}")
            , zip3 (repeat Port) [[largeTown, city, largeCity, hugeCity1]] (repeat "^\\s+}")
            , zip3 (repeat Caravan) [[largeTown, largeCity]] (repeat "^\\s+trade_level")
            ]
        recruitmentsBasicGuilds = map makeRegexSnippets . zip (repeat ".+?capability.+?\\{\\r\\n") $ concat
            [ zip3 (repeat AssassinsMuslimGuild) [[city, largeCity, hugeCity1]] (repeat "^\\s+}")
            , zip3 (repeat MasonsGuild) [[city, largeCity, hugeCity1]] (repeat "^\\s+construction")
            , zip3 (repeat MerchantsGuild) [[city, largeCity, hugeCity1]] (repeat "^\\s+trade_base")
            , zip3 (repeat TemplarsChapterHouse) [[city, largeCity, hugeCity1]] (repeat "^\\s+}")
            , zip3 (repeat StJohnsChapterHouse) [[city, largeCity, hugeCity1]] (repeat "^\\s+}")
            , zip3 (repeat TeutonicKnightsChapterHouse) [[city, largeCity, hugeCity1]] (repeat "^\\s+}")
            , zip3 (repeat KnightsOfSantiagoChapterHouse) [[city, largeCity, hugeCity1]] (repeat "^\\s+}")
            ]
        makeRegexSnippets (startpoint, (building, levels, endpoint)) = [buildingRegex]
            ++ concatMap makeSnippetInit (init levels)
            ++ makeSnippetLast (last levels)
            where
                buildingRegex =
                    ("^building\\s+" ++ show building ++ startpoint, id)
                makeSnippetInit level =
                    [ ("\\s+recruit_pool.+?", only $ getRecruits building level)
                    , (endpoint ++ ".+?capability.+?\\{\\r\\n", id)
                    ]
                makeSnippetLast level =
                    [ ("\\s+recruit_pool.+?", only $ getRecruits building level)
                    , (endpoint, id)
                    ]
        recruitment =
                -- regular regex buildings
                recruitmentsBasic
                ++
                recruitmentsBasicGuilds
                ++
                -- irregular regex buildings
                [
                    -- castle_port
                    [ ("^building\\s+castle_port.+?            \\{\\r\\n", id)
                    , ("\\s+recruit_pool.+?", only $ getRecruits Port largeTown)
                    , ("                trade.+?\\r\\n^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit_pool.+?", only $ getRecruits Port city)
                    , ("                trade.+?\\r\\n^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit_pool.+?", only $ getRecruits Port largeCity)
                    , ("                trade.+?\\r\\n^            }.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit_pool.+?", only $ getRecruits Port hugeCity1)
                    , ("                trade.+?\\r\\n^            }", id)
                    ]
                ,
                    -- markets (transilvanian peasants --- hungary)
                    [ ("^building\\s+market.+?capability.+?capability.+?capability.+?", id)
                    , ("trade_base.+?\\r\\n", add $ getRecruits Tavern city)
                    , (".+?capability.+?", id)
                    , ("trade_base.+?\\r\\n", add $ getRecruits Tavern largeCity)
                    , (".+?capability.+?", id)
                    , ("trade_base.+?\\r\\n", add $ getRecruits Tavern hugeCity1)
                    ]
                ,
                    -- city_hall
                    [ ("^building\\s+city_hall.+?            \\{", id)
                    , ("\\r\\n", add $ getRecruits CityHall largeTown)
                    , (".+?diplomat[^\\r]+?southern_european.+?\\r\\n", id)
                    , ("\\s+recruit_pool.+?\\r\\n.+?\\r\\n", only $ getRecruits CityHall city)
                    , (".+?diplomat[^\\r]+?southern_european.+?\\r\\n", id)
                    , ("\\s+recruit_pool.+?\\r\\n.+?\\r\\n.+?\\r\\n", only $ getRecruits CityHall largeCity)
                    , (".+?diplomat[^\\r]+?southern_european.+?\\r\\n", id)
                    , ("\\s+recruit_pool.+?\\r\\n.+?\\r\\n.+?\\r\\n.+?\\r\\n.+?\\r\\n", only $ getRecruits CityHall hugeCity1)
                    ]
                ,
                    -- bullring (jinetes)
                    [ ("^building\\s+bullring.+?            \\{\\r\\n", id)
                    , ("\\s+recruit_pool.+?", only $ getRecruits Bullring city)
                    , ("^                happiness", id)
                    ]
                ,
                    -- guild_assassins_guild (battlefield assassins --- hungary)
                    [ ("^building\\s+guild_assassins_guild.+?capability.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit_pool.+?\\r\\n", only $ getRecruits AssassinsGuild largeCity)
                    , ("^                law_bonus.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit_pool.+?\\r\\n", only $ getRecruits AssassinsGuild hugeCity1)
                    , ("^                law_bonus", id)
                    ]
                ,
                    -- guild_woodsmens_guild
                    [ ("^building\\s+guild_woodsmens_guild.+?capability.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit_pool.+?\\r\\n", only $ getRecruits WoodsmensGuild largeCity)
                    , ("^                archer_bonus.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit_pool.+?\\r\\n", only $ getRecruits WoodsmensGuild hugeCity1)
                    , ("^                archer_bonus", id)
                    ]
                ]

_M2TW_EDBE_FUNCS :: RegexSets
_M2TW_EDBE_FUNCS = addTrueTest [r1, r2] ++ addTrueTest (thieves ++ farms ++ taverns ++ wharves)
    where
        -- Remove all references to mines+1 and c_mines+1
        r1 =    [ ("^mines\\+.+?\\r\\n", nil)
                ]
        r2 =    [ ("^c_mines\\+.+?\\r\\n", nil)
                ]
        -- Remove all references to thieves guild buildings
        thieves =
            [
                [ ("^[^\\r]*?thieves.+?\\r\\n", nil)
                ]
            ,
                [ ("^guild_thiefs.+?\\r\\n", nil)
                ]
            ]
        -- Remove all references to farms.
        farms =
            [
                [ ("^farms.+?\\r\\n", nil)
                ]
            ,
                [ ("^hinterland_farms.+?\\r\\n", nil)
                ]
            ]
        -- Remove all references to taverns.
        taverns =
            [
                [ ("^brothel.+?\\r\\n", nil)
                ]
            ,
                [ ("^inn.+?\\r\\n", nil)
                ]
            ,
                [ ("^tavern.+?\\r\\n", nil)
                ]
            ,
                [ ("^coaching.+?\\r\\n", nil)
                ]
            ,
                [ ("^pleasure.+?\\r\\n", nil)
                ]
            ,
                [ ("^taverns_name.+?\\r\\n", nil)
                ]
            ]
        -- Remove all references to wharves
        wharves =
            [
                [ ("^merchants_wharf.+?\\r\\n", nil)
                ]
            ,
                [ ("^warehouse.+?\\r\\n", nil)
                ]
            ,
                [ ("^docklands.+?\\r\\n", nil)
                ]
            ,
                [ ("^sea_trade.+?\\r\\n", nil)
                ]
            ]

_M2TW_EDG_FUNCS :: RegexSets
_M2TW_EDG_FUNCS = addTrueTest [r1] ++ addTrueTest (taverns ++ wharves)
    where
        -- Remove thieves guild.
        r1 =    [ ("^Guild thiefs.+?;-+\\r\\n", nil)
                ]
        -- Change tavern buildings references into markets.
        taverns =
            [
                [ ("brothel", only "corn_exchange")
                ]
            ,
                [ ("inn", only "market")
                ]
            ,
                [ ("tavern", only "fairground")
                ]
            ,
                [ ("coaching_house", only "great_market")
                ]
            ,
                [ ("pleasure_palace", only "merchants_quarter")
                ]
            ]
        -- Change wharves buildings references into ports.
        wharves =
            [
                [ ("merchants_wharf", only "shipwright")
                ]
            ,
                [ ("warehouse", only "dockyard")
                ]
            ,
                [ ("docklands", only "naval_drydock")
                ]
            ]

_M2TW_EDU_FUNCS :: RegexSets
_M2TW_EDU_FUNCS = addTrueTest [r1, r2, r3] ++ [rN]
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
