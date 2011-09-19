module Regex.RTW where

import Regex
import qualified Data.ByteString.Char8 as BC
import Data.Recruit.RTW

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

_RTW_DS_FUNCS :: RegexSets
_RTW_DS_FUNCS = addTrueTest [r1, r2] ++ addTrueTest (noSpies) ++ addTrueTest [giveRoads] ++ giveRoads'
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

_RTW_EDB_FUNCS :: RegexSets
_RTW_EDB_FUNCS = addTrueTest $ [r1]
    ++ recruitment
    where
        -- All building constructions take 1 turn
        r1 =    [ ("^\\s+construction\\s+", id)
                , (_REGEX_INT, only "1")
                ]
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
