module Regex.RTW where

import Regex
import Data.Recruit.RTW

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
                    , ("\\s+recruit.+?", only $ getRecruits Core 0 ++ diplomats)
                    , ("^\\s+}.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Core town ++ diplomats)
                    , ("^\\s+}.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Core largeTown ++ diplomats)
                    , ("^\\s+upgrade_bodyguard.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Core city ++ diplomats)
                    , ("^\\s+upgrade_bodyguard.+?capability.+?\\{\\r\\n", id)
                    , ("\\s+recruit.+?", only $ getRecruits Core largeCity ++ diplomats)
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
        -- Need to add back in the diplomat recruitment slots, since we deleted
        -- them for core_building with *recruitment* function.
        diplomats = concatMap (\(a, b) -> a ++ "{ " ++ b ++ ", }\r\n") $ zip (repeat diplomat)
            [ "barbarian"
            , "carthaginian"
            , "eastern"
            , "egyptian"
            , "greek"
            , "roman"
            ]
        diplomat = replicate 16 ' ' ++ "agent diplomat 0 requires factions "
