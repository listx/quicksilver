{-# LANGUAGE RecordWildCards #-}

module Data.Recruit.RTW where

import Util

data RecruitPool = R
	{ unitName :: String
	, experience :: Int
	, req :: String
	}

instance Show RecruitPool where
	show R{..} = "                recruit "
		++ dquote unitName ++ " "
		++ show experience ++ " "
		++ req

data Building
	= Core
	| Barracks
	| Equestrian
	| Missiles
	| Market
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
		Market -> "market"
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
town        = 0 -- town
largeTown   = 1 -- large_town
city        = 2 -- city ("Minor City" in-game)
largeCity   = 3 -- large_city
hugeCity    = 4 -- huge_city

modRecruit :: Int -> RecruitPool -> String
modRecruit e R{..} = show R
	{ unitName = unitName
	, experience = experience + e
	, req = req
	}

getRecruits :: Building -> Int -> String
getRecruits btype rmod = concatMap
	(\s -> modRecruit rmod s ++ "\r\n")
	(map snd . filter (\(a, _) -> a == btype)
		$ map (\(b, rp@R{..}) -> (b, rp {req = "requires factions " ++ req}))
		_RECRUIT_DATA)

_RECRUIT_DATA :: [(Building, RecruitPool)]
_RECRUIT_DATA =
	zip (repeat Core)
	[ R "carthaginian peasant" 0 "{ spain, }"
	, R "barb peasant briton" 0 "{ britons, }"
	, R "barb peasant dacian" 0 "{ dacia, }"
	, R "barb peasant gaul" 0 "{ gauls, }"
	, R "barb peasant german" 0 "{ germans, }"
	, R "barb peasant scythian" 0 "{ scythia, }"
	, R "carthaginian peasant" 0 "{ carthaginian, }"
	, R "east peasant" 0 "{ eastern, }"
	, R "egyptian peasant" 0 "{ egyptian, }"
	, R "greek peasant" 0 "{ greek, }"
	, R "roman peasant" 0 "{ roman, }"
	, R "roman praetorian cohort i" 0 "{ roman, }  and marian_reforms"
	]
	++ zip (repeat Barracks)
	[ R "carthaginian city militia" 0 "{ spain, }"
	, R "carthaginian infantry" 0 "{ spain, }"
	, R "spanish scutarii" 0 "{ spain, }"
	, R "barb infantry briton" 0 "{ britons, }"
	, R "barb infantry dacian" 0 "{ dacia, }"
	, R "barb infantry gaul" 0 "{ gauls, }"
	, R "warband spear german" 0 "{ germans, }"
	, R "barb horse archers scythian" 0 "{ scythia, }"
	, R "warband sword briton" 0 "{ britons, }"
	, R "warband sword gaul" 0 "{ gauls, }"
	, R "warband axe german" 0 "{ germans, }"
	, R "warband falx dacian" 0 "{ dacia, }"
	, R "barb warguard gaul" 0 "{ gauls, }"
	, R "barb warguard briton" 0 "{ britons, }"
	, R "barb warguard dacian" 0 "{ dacia, }"
	, R "warband dhaxe german" 0 "{ germans, }"
	, R "warband axe scythian" 1 "{ scythia, }"
	, R "carthaginian city militia" 0 "{ carthage, }"
	, R "numidian javelinmen" 0 "{ numidia, }"
	, R "carthaginian infantry" 0 "{ carthage, }"
	, R "numidian desert warriors" 1 "{ numidia, }"
	, R "carthaginian medium infantry" 0 "{ carthage, }"
	, R "carthaginian heavy infantry" 0 "{ carthage, }"
	, R "numidian legionaries" 0 "{ numidia, }"
	, R "east hillmen" 0 "{ eastern, }"
	, R "east infantry" 0 "{ eastern, }"
	, R "east heavy infantry" 0 "{ armenia, }"
	, R "east hoplite" 0 "{ pontus, }"
	, R "east hoplite brazen shield" 0 "{ pontus, }"
	, R "east legionary" 0 "{ armenia, }"
	, R "egyptian nubian spearmen" 0 "{ egyptian, }"
	, R "egyptian nile infantry" 0 "{ egyptian, }"
	, R "egyptian infantry" 0 "{ egyptian, }"
	, R "egyptian elite guards" 0 "{ egyptian, }"
	, R "greek hoplite militia" 0 "{ greek, }"
	, R "greek levy pikemen" 0 "{ seleucid, macedon, }"
	, R "warband falx thracian" 0 "{ thrace, }"
	, R "greek hoplite" 1 "{ greek_cities, }"
	, R "greek pikemen" 0 "{ thrace, seleucid, macedon, }"
	, R "greek hoplite elite" 0 "{ greek_cities, }"
	, R "greek royal pikemen" 0 "{ macedon, }"
	, R "greek silver shield pikemen" 0 "{ seleucid, }"
	, R "greek hoplite spartan" 0
		"{ greek_cities, }  and hidden_resource sparta"
	, R "greek bastarnae" 0 "{ thrace, }"
	, R "greek argyraspid" 0 "{ seleucid, }"
	, R "roman city militia" 0 "{ roman, }"
	, R "roman hastati" 0 "{ roman, }  and not marian_reforms"
	, R "roman infantry auxillia" 0 "{ roman, }  and marian_reforms"
	, R "roman princeps" 0 "{ roman, }  and not marian_reforms"
	, R "roman legionary cohort i" 0 "{ roman, }  and marian_reforms"
	, R "roman legionary first cohort i" 0
		"{ roman, }  and hidden_resource rome and marian_reforms"
	, R "roman triarii" 1 "{ roman, }  and not marian_reforms"
	, R "roman legionary cohort ii" 0 "{ roman, }  and marian_reforms"
	, R "roman legionary first cohort ii" 0
		"{ roman, }  and hidden_resource rome and marian_reforms"
	, R "roman praetorian cohort urban i" 0 "{ roman, }  and marian_reforms"
	, R "roman praetorian cohort i" 0 "{ roman, }  and marian_reforms"
	]
	++ zip (repeat Equestrian)
	[ R "carthaginian warhounds" 0 "{ spain, }"
	, R "carthaginian cavalry" 0 "{ spain, }"
	, R "carthaginian medium cavalry" 0 "{ spain, }"
	, R "barb cavalry gaul" 0 "{ gauls, }"
	, R "barb cavalry german" 0 "{ germans, }"
	, R "barb cavalry dacian" 0 "{ dacia, }"
	, R "barb cavalry scythian" 0 "{ scythia, }"
	, R "barb wardogs briton" 0 "{  britons, }"
	, R "barb wardogs dacian" 0 "{ dacia, }"
	, R "barb wardogs gaul" 0 "{ gauls, }"
	, R "barb wardogs german" 0 "{ germans, }"
	, R "barb wardogs scythian" 0 "{ scythia, }"
	, R "barb noble cavalry gaul" 0 "{ gauls, }"
	, R "barb noble cavalry german" 0 "{ germans, }"
	, R "barb noble cavalry dacian" 0 "{ dacia, }"
	, R "barb scythian nobles scythian" 0 "{ scythia, }"
	, R "barb noble horse archers scythian" 0 "{ scythia, }"
	, R "carthaginian cavalry" 0 "{ carthage, }"
	, R "numidian cavalry" 0 "{ numidia, }"
	, R "carthaginian medium cavalry" 0 "{ carthaginian, }"
	, R "carthaginian elephant forest" 0 "{ carthage, }  and resource elephants"
	, R "carthaginian elephant african" 0
		"{ carthage, }  and resource elephants"
	, R "carthaginian elephant african cataphract" 0
		"{ carthage, }  and resource elephants"
	, R "numidian camel riders" 0 "{ numidia, }  and resource camels"
	, R "carthaginian royal cavalry" 0 "{ carthage, }"
	, R "east cavalry" 0 "{ pontus, }"
	, R "east horse archer" 0 "{ armenia, parthia, }"
	, R "east heavy cavalry" 0 "{ pontus, }"
	, R "east persian cavalry" 0 "{ parthia, }"
	, R "east cataphract archer" 0 "{ armenia, }"
	, R "east cappodocian cavalry" 0 "{ pontus, }"
	, R "east heavy cataphract" 0 "{ armenia, parthia, }"
	, R "east elephant" 0 "{ parthia, }  and resource elephants"
	, R "east camel cataphract" 0 "{ parthia, }  and resource camels"
	, R "egyptian cavalry" 0 "{ egyptian, }"
	, R "egyptian nubian cavalry" 0 "{ egyptian, }"
	, R "egyptian cleruch" 0 "{ egyptian, }"
	, R "egyptian bedouin" 0 "{ egyptian, }  and resource camels"
	, R "greek light lancer" 0 "{ macedon, }"
	, R "greek cavalry" 0 "{ seleucid, thrace, greek_cities, }"
	, R "greek medium cavalry" 0 "{ greek, }"
	, R "greek elephant small" 0 "{ seleucid, }  and resource elephants"
	, R "greek heavy cavalry" 0 "{ macedon, }"
	, R "east heavy cataphract" 0 "{ seleucid, }"
	, R "greek elephant african" 0 "{ seleucid, }  and resource elephants"
	, R "greek royal cavalry" 0 "{ seleucid, macedon, }"
	, R "greek elephant cataphract" 0 "{ seleucid, }  and resource elephants"
	, R "greek incendiary pigs" 0 "{ greek_cities, }"
	, R "roman cavalry auxillia" 0 "{ roman, }"
	, R "roman light cavalry" 2 "{ roman, }  and not marian_reforms"
	, R "roman medium cavalry" 0 "{ roman, }  and marian_reforms"
	, R "roman heavy cavalry" 0 "{ roman, }  and marian_reforms"
	, R "roman praetorian cavalry" 0 "{ roman, }  and marian_reforms"
	, R "roman wardogs" 0 "{ roman, }"
	, R "roman pigs" 0 "{ roman, }"
	]
	++ zip (repeat Missiles)
	[ R "carthaginian slinger" 0 "{ spain, }"
	, R "carthaginian peltast" 0 "{ spain, }"
	, R "barb peltast gaul" 0 "{ gauls, }"
	, R "barb peltast german" 0 "{ germans, }"
	, R "barb slinger briton" 0 "{ britons, }"
	, R "barb archer dacian" 0 "{ dacia, }"
	, R "barb archer scythian" 0 "{ scythia, }"
	, R "warband archer german" 0 "{ germans, }"
	, R "warband archer dacian" 0 "{ dacia, }"
	, R "warband archer scythian" 0 "{ scythia, }"
	, R "warband huntsman gaul" 0 "{ gauls, }"
	, R "barb ballista dacian" 0 "{ dacia, }"
	, R "barb onager dacian" 0 "{ dacia, }"
	, R "barb onager scythian" 0 "{ scythia, }"
	, R "carthaginian peltast" 0 "{ carthage, }"
	, R "carthaginian archer" 0 "{ numidia, }"
	, R "carthaginian slinger" 0 "{ carthaginian, }"
	, R "carthaginian onager" 0 "{ carthaginian, }"
	, R "carthaginian heavy onager" 0 "{ carthage, }"
	, R "east peltast" 0 "{ armenia, pontus, }"
	, R "east slinger" 0 "{ parthia, }"
	, R "east archer" 0 "{ eastern, }"
	, R "east onager" 0 "{ eastern, }"
	, R "egyptian peltast" 0 "{ egyptian, }"
	, R "egyptian slingers" 0 "{ egyptian, }"
	, R "egyptian archer" 0 "{ egyptian, }"
	, R "egyptian archer elite" 0 "{ egyptian, }"
	, R "egyptian onager" 0 "{ egyptian, }"
	, R "egyptian heavy onager" 0 "{ egyptian, }"
	, R "greek peltast" 0 "{ greek, }"
	, R "greek archer" 0 "{ greek, }"
	, R "greek ballista" 0 "{ macedon, greek_cities, }"
	, R "greek heavy peltast" 0 "{ greek_cities, }"
	, R "greek onager" 0 "{ greek, }"
	, R "greek heavy onager" 0 "{ macedon, greek_cities, }"
	, R "roman velite" 0 "{ roman, }  and not marian_reforms"
	, R "roman light infantry auxillia" 0 "{ roman, }  and marian_reforms"
	, R "roman archer" 0 "{ roman, }  and not marian_reforms"
	, R "roman archer auxillia" 0 "{ roman, }  and marian_reforms"
	, R "roman ballista" 0 "{ roman, }"
	, R "roman scorpion" 0 "{ roman, }"
	, R "roman onager" 0 "{ roman, }"
	, R "roman heavy onager" 0 "{ roman, }"
	, R "roman repeating ballista" 0 "{ roman, }"
	]
	++ zip (repeat Smith)
	[ R "barb chariot heavy briton" 0 "{ britons, }"
	, R "barb chariot light briton" 0 "{ britons, }"
	, R "east scythed chariot" 0 "{ pontus, }"
	, R "east chariot archer" 0 "{ pontus, }"
	, R "egyptian chariot" 0 "{ egyptian, }"
	, R "egyptian chariot archer" 0 "{ egyptian, }"
	, R "rebel amazon chariots" 2 "{ slave, }"
	, R "greek chariot scythed" 0 "{ seleucid, }"
	]
	++ zip (repeat Port)
	[ R "naval boats" 0 "{ barbarian, }"
	, R "naval large boats" 0 "{ barbarian, }"
	, R "naval biremes" 0 "{ carthaginian, }"
	, R "naval triremes" 0 "{ carthaginian, }"
	, R "naval quinquiremes" 0 "{ carthaginian, }"
	, R "naval biremes" 0 "{ eastern, }"
	, R "naval triremes" 0 "{ eastern, }"
	, R "naval quinquiremes" 0 "{ eastern, }"
	, R "naval biremes" 0 "{ egyptian, }"
	, R "naval triremes" 0 "{ egyptian, }"
	, R "naval quinquiremes" 0 "{ egyptian, }"
	, R "naval biremes" 0 "{ greek, }"
	, R "naval triremes" 0 "{ greek, }"
	, R "naval quinquiremes" 0 "{ greek, }"
	, R "naval biremes" 0 "{ roman, }"
	, R "naval triremes" 0 "{ roman, }"
	, R "naval quinquiremes" 0 "{ roman, }"
	]
	++ zip (repeat Amphitheatres)
	[ R "roman velite gladiator" 0 "{ romans_brutii, }"
	, R "roman samnite gladiator" 0 "{ romans_senate, romans_julii, }"
	, R "roman mirmillo gladiator" 0 "{ romans_scipii, }"
	]
	++ zip (repeat TempleOfBattle)
	[ R "barb naked fanatics dacian" 1 "{ dacia, }"
	]
	++ zip (repeat TempleOfBattleforge)
	[ R "barb naked fanatics gauls" 1 "{ gauls, }"
	]
	++ zip (repeat TempleOfFertility)
	[ R "barb screeching women german" 0 "{ germans, }"
	]
	++ zip (repeat TempleOfHealing)
	[ R "barb druids briton" 0 "{ britons, }"
	]
	++ zip (repeat TempleOfJustice)
	[ R "barb druids gaul" 0 "{ gauls, }"
	, R "barb naked fanatics spain" 0 "{ spain, }"
	, R "spanish bull warriors" 0 "{ spain, }"
	, R "carthaginian sacred band infantry" 0 "{ carthage, }"
	]
	++ zip (repeat TempleOfLaw)
	[ R "roman arcani" 0 "{ romans_scipii, }"
	]
	++ zip (repeat TempleOfLeadership)
	[ R "roman arcani" 0 "{ romans_julii, }"
	]
	++ zip (repeat TempleOfLove)
	[ R "barb head hunting maidens scythian" 0 "{ scythia, }"
	, R "barb scythian noblewomen scythian" 0 "{  scythia, }"
	]
	++ zip (repeat TempleOfNaval)
	[ R "naval deceres" 0 "{ roman, }"
	, R "naval corvus quinquireme" 0 "{ roman, }"
	]
	++ zip (repeat TempleOfVictory)
	[ R "warband woad briton" 1 "{ britons, }"
	, R "warband hurler briton" 1 "{ britons, }"
	]
	++ zip (repeat TempleOfViolence)
	[ R "barb berserker german" 0 "{ germans, }"
	, R "roman arcani" 0 "{ romans_brutii, }"
	]
	++ zip (repeat TempleOfViking)
	[ R "barb naked fanatics german" 0 "{ germans, }"
	, R "barb gothic cavalry german" 0 "{ germans, }"
	]
