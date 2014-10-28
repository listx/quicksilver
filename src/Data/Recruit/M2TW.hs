{-# LANGUAGE RecordWildCards #-}

module Data.Recruit.M2TW where

import Util

data RecruitPool = R
	{ unitName :: String
	, pointsInitial :: Int -- starting point (units available upon completion of building)
	, pointsGrowth :: Double -- points gained per turn (set to 0.000001 by Creative Assembly to only allow retraining, not recruiting)
	, pointsCap :: Double -- maximum build points possible (set to 0.999 by Creative Assembly to only allow retraining, not recruiting)
	, experience :: Int -- starting experience of the unit (0 to 9)
	, req :: String -- required things (e.g., "requires factions { england, scotland...")
	}

instance Show RecruitPool where
	show R{..} = "                recruit_pool "
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
		KnightsOfSantiagoChapterHouse ->
			"guild_knights_of_santiago_chapter_house"
		_                             -> "guild_woodsmens_guild"


village
	, town
	, largeTown
	, city
	, largeCity
	, hugeCity1
	, hugeCity2 :: (Int, Double, Int, Int)
village     = (0, 0.00, 0, 0) -- village
town        = (0, 0.10, 0, 0) -- town
largeTown   = (1, 0.15, 1, 1) -- large_town
city        = (2, 0.20, 2, 2) -- city ("Minor City" in-game)
largeCity   = (3, 0.25, 3, 3) -- large_city
hugeCity1   = (4, 0.30, 4, 4) -- huge_city
hugeCity2   = (5, 0.35, 5, 5) -- huge_city

-- recruitment:
--      recruit_pool
--      UNIT_TYPE
--    b starting point (units available upon completion of building)
--    g points gained per turn
--    c maximum points possible
--    e starting experience of the unit (0 to 9)
--      restrict to certain factions

modRecruit :: (Int, Double, Int, Int) -> RecruitPool -> String
modRecruit (p, g, c ,e) R{..} = show R
	{ unitName = unitName
	, pointsInitial = pointsInitial + p
	, pointsGrowth = if pointsGrowth /= 0.000001
		then pointsGrowth + g
		else pointsGrowth
	, pointsCap = if pointsCap /= 0.999
		then pointsCap + (fromIntegral c)
		else pointsCap
	, experience = experience + e
	, req = req
	}

getRecruits :: Building -> (Int, Double, Int, Int) -> String
getRecruits btype rmod = concatMap (\s -> modRecruit rmod s ++ "\r\n")
	(map snd
		. filter (\(a, _) -> a == btype)
		$ map (\(b, rp@R{..}) -> (b, rp {req = "requires factions " ++ req}))
		_RECRUIT_DATA)

_RECRUIT_DATA :: [(Building, RecruitPool)]
_RECRUIT_DATA =
	zip (repeat Core)
	[ R "Conquistadores" 1 0.5 4 1 "{ spain, portugal, }  and hidden_resource america"
	, R "Swiss Guard" 1 0.5 4 0 "{ papal_states, }"
	, R "Hussars" 1 0.5 4 0 "{ poland, hungary, }"
	, R "Cossack Musketeers" 1 0.5 4 0 "{ russia, }  and event_counter gunpowder_discovered 1"
	, R "Dismounted Conquistadores" 1 0.5 4 1 "{ spain, portugal, }  and hidden_resource america"
	, R "Town Militia" 1 0.2 2 0 "{ england, scotland, france, hre, denmark, spain, portugal, Normans, }"
	, R "Italian Militia" 1 0.2 2 0 "{ milan, venice, papal_states, sicily, }"
	, R "EE Town Militia" 1 0.2 2 0 "{ poland, hungary, }"
	, R "EE Archer Militia" 1 0.2 2 0 "{ russia, }"
	, R "SE Town Militia" 1 0.2 2 0 "{ byzantium, }"
	, R "ME Town Militia" 1 0.2 2 0 "{ moors, turks, mongols, timurids, }"
	, R "ME Archer Militia" 1 0.2 2 0 "{ egypt, }"
	, R "Peasant Spearmen" 1 0.2 2 0 "{ Saxons, }"
	, R "Spear Militia" 0 0.000001 0.999 0 "{ england, scotland, france, hre, denmark, spain, portugal, Normans, }"
	, R "Italian Spear Militia" 0 0.000001 0.999 0 "{ milan, venice, papal_states, sicily, }"
	, R "EE Spear Militia" 0 0.000001 0.999 0 "{ poland, russia, hungary, }"
	, R "SE Spear Militia" 0 0.000001 0.999 0 "{ byzantium, }"
	, R "ME Spear Militia" 0 0.000001 0.999 0 "{ moors, egypt, turks, mongols, timurids, }"
	, R "Archer Militia" 0 0.000001 0.999 0 "{ england, }"
	, R "Scots Pike Militia" 0 0.000001 0.999 0 "{ scotland, }"
	, R "Crossbow Militia" 0 0.000001 0.999 0 "{ france, hre, denmark, spain, portugal, }"
	, R "Genoese Crossbow Militia" 0 0.000001 0.999 0 "{ milan, }"
	, R "Pavise Crossbow Militia" 0 0.000001 0.999 0 "{ venice, papal_states, sicily, hungary, }"
	, R "EE Crossbow Militia" 0 0.000001 0.999 0 "{ poland, russia, }"
	, R "S Archer Militia" 0 0.000001 0.999 0 "{ byzantium, }"
	, R "ME Crossbow Militia" 0 0.000001 0.999 0 "{ moors, }"
	, R "Saracen Militia" 0 0.000001 0.999 0 "{ egypt, turks, }"
	, R "Sabadar Militia" 0 0.000001 0.999 0 "{ timurids, }"
	, R "Mongol Horse Archers" 1 0.4 3 0 "{ mongols, }"
	, R "Mounted Sergeants" 1 0.4 3 0 "{ Normans, }"
	, R "Theigns" 1 0.4 3 0 "{ Saxons, }"
	, R "Dismounted Broken Lances" 1 0.4 3 0 "{ milan, }"
	, R "Noble Pikemen" 1 0.4 3 0 "{ scotland, }"
	, R "Scots Guard" 1 0.4 3 0 "{ france, }"
	, R "Christian Guard" 1 0.4 3 0 "{ moors, }"
	, R "Broken Lances" 1 0.5 4 0 "{ milan, venice, papal_states, }"
	]
	++ zip (repeat Castle)
	[ R "Dismounted Polish Knights" 1 0.4 3 0 "{ poland, }"
	, R "Khan's Guard" 1 0.4 3 0 "{ mongols, timurids, }"
	, R "Noble Swordsmen" 1 0.4 3 0 "{ scotland, }"
	, R "Noble Knights" 1 0.4 3 0 "{ france, }"
	, R "Imperial Knights" 1 0.4 3 0 "{ hre, }"
	, R "Chivalric Knights" 1 0.4 3 0 "{ spain, sicily, }"
	, R "Italian MAA" 1 0.4 3 0 "{ milan, venice, papal_states, }"
	, R "Polish Retainers" 1 0.4 3 0 "{ poland, }"
	, R "E Chivalric Knights" 1 0.4 3 0 "{ hungary, }"
	, R "Byzantine Lancers" 1 0.4 3 0 "{ byzantium, }"
	, R "Granadine Lancers" 1 0.4 3 0 "{ moors, }"
	, R "Mongol Heavy Lancers" 1 0.4 3 0 "{ mongols, timurids, }"
	, R "Feudal Knights" 1 0.4 3 0 "{ scotland, denmark, }"
	, R "Polish Knights" 1 0.4 3 0 "{ poland, }"
	, R "Boyar Sons" 1 0.4 3 0 "{ russia, }"
	, R "Vardariotai" 1 0.4 3 0 "{ byzantium, }"
	, R "Granadine Jinetes" 1 0.4 3 0 "{ moors, }"
	, R "Mamluks" 1 0.4 3 0 "{ egypt, }"
	, R "Sipahi Lancers" 1 0.4 3 0 "{ turks, }"
	, R "Mongol Heavy Archers" 1 0.4 3 0 "{ mongols, timurids, }"
	, R "Dismounted Polish Nobles" 1 0.4 3 0 "{ poland, }"
	, R "Mailed Knights" 1 0.4 3 0 "{ Normans, }"
	, R "English Huscarls" 1 0.4 3 0 "{ Saxons, }"
	, R "Hobilars" 1 0.4 3 0 "{ england, }"
	, R "Border Horse" 1 0.4 3 0 "{ scotland, }"
	, R "Mounted Sergeants" 1 0.4 3 0 "{ france, hre, milan, venice, papal_states, sicily, Normans, }"
	, R "Scouts" 1 0.4 3 0 "{ denmark, }"
	, R "Jinetes" 1 0.4 3 0 "{ spain, portugal, }"
	, R "Polish Shooters" 1 0.4 3 0 "{ poland, }"
	, R "Kazaks" 1 0.4 3 0 "{ russia, }"
	, R "Magyar Cavalry" 1 0.4 3 0 "{ hungary, }"
	, R "Skythikon" 1 0.4 3 0 "{ byzantium, }"
	, R "Desert Cavalry" 1 0.4 3 0 "{ moors, }"
	, R "Arab Cavalry" 1 0.4 3 0 "{ egypt, }"
	, R "Turkomans" 1 0.4 3 0 "{ turks, }"
	, R "Mongol Horse Archers" 1 0.4 3 0 "{ mongols, }"
	, R "Turkish Horse Archers" 1 0.4 3 0 "{ timurids, }"
	, R "Theigns" 1 0.4 3 0 "{ Saxons, }"
	, R "Peasants" 0 0.000001 0.999 0 "{ england, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, }"
	, R "Highland Rabble" 0 0.000001 0.999 0 "{ scotland, }"
	, R "Southern Peasants" 0 0.000001 0.999 0 "{ sicily, byzantium, }"
	, R "EE Peasants" 0 0.000001 0.999 0 "{ poland, russia, hungary, }"
	, R "ME Peasants" 0 0.000001 0.999 0 "{ moors, egypt, turks, mongols, timurids, }"
	, R "Levy Spearmen" 0 0.000001 0.999 0 "{ england, }"
	, R "Highlanders" 0 0.000001 0.999 0 "{ scotland, }"
	, R "Sergeant Spearmen" 0 0.000001 0.999 0 "{ france, hre, milan, venice, papal_states, sicily, Normans, }"
	, R "Viking Raiders" 0 0.000001 0.999 0 "{ denmark, }"
	, R "Javelinmen" 0 0.000001 0.999 0 "{ spain, }"
	, R "Lusitanian Javelinmen" 0 0.000001 0.999 0 "{ portugal, }"
	, R "Woodsmen" 0 0.000001 0.999 0 "{ poland, russia, }"
	, R "Slav Levies" 0 0.000001 0.999 0 "{ hungary, }"
	, R "Byzantine Spearmen" 0 0.000001 0.999 0 "{ byzantium, }"
	, R "Berber Spearmen" 0 0.000001 0.999 0 "{ moors, }"
	, R "Kurdish Javelinmen" 0 0.000001 0.999 0 "{ egypt, }"
	, R "Turkish Javelinmen" 0 0.000001 0.999 0 "{ turks, }"
	, R "ME Levy Spearmen" 0 0.000001 0.999 0 "{ mongols, }"
	, R "Afghan Javelinmen" 0 0.000001 0.999 0 "{ timurids, }"
	, R "Peasant Spearmen" 0 0.000001 0.999 0 "{ Saxons, }"
	, R "Peasant Archers" 0 0.000001 0.999 0 "{ england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, }"
	, R "S Peasant Archers" 0 0.000001 0.999 0 "{ sicily, hungary, byzantium, }"
	, R "EE Peasant Archers" 0 0.000001 0.999 0 "{ poland, russia, }"
	, R "Sudanese Javelinmen" 0 0.000001 0.999 0 "{ moors, }"
	, R "ME Peasant Archers" 0 0.000001 0.999 0 "{ egypt, turks, mongols, timurids, }"
	, R "Dismounted Feudal Knights" 1 0.4 3 0 "{ scotland, denmark, }"
	, R "Dismounted Noble Knights" 1 0.4 3 0 "{ france, }"
	, R "Dismounted Imperial Knights" 1 0.4 3 0 "{ hre, }"
	, R "Dismounted Chivalric Knights" 1 0.4 3 0 "{ spain, }"
	, R "Dismounted Italian MAA" 1 0.4 3 0 "{ milan, venice, papal_states, }"
	, R "Dismounted Norman Knights" 1 0.4 3 0 "{ sicily, }"
	, R "Dismounted Boyar Sons" 1 0.4 3 0 "{ russia, }"
	, R "Dismounted E Chivalric Knights" 1 0.4 3 0 "{ hungary, }"
	, R "Dismounted Arab Cavalry" 1 0.4 3 0 "{ moors, egypt, }"
	, R "Dismounted Sipahi Lancers" 1 0.4 3 0 "{ turks, }"
	, R "Dismounted Heavy Archers" 1 0.4 3 0 "{ mongols, timurids, }"
	, R "Dismounted English Knights" 1 0.5 4 0 "{ england, }"
	, R "Dismounted Portuguese Knights" 1 0.5 4 0 "{ portugal, }"
	, R "Dismounted Feudal Knights" 1 0.7 6 0 "{ england, france, hre, spain, portugal, milan, venice, papal_states, hungary, }"
	, R "Arab Cavalry" 1 0.7 6 0 "{ moors, }"
	, R "Mamluk Archers" 1 0.7 6 0 "{ egypt, }"
	, R "Sipahis" 1 0.7 6 0 "{ turks, }"
	, R "Mongol Light Lancers" 1 0.7 6 0 "{ mongols, }"
	, R "Turkomans" 1 0.7 6 0 "{ timurids, }"
	, R "Mailed Knights" 1 0.4 3 0 "{ england, scotland, }"
	, R "Feudal Knights" 1 0.4 3 0 "{ france, hre, spain, milan, venice, papal_states, }"
	, R "Dismounted Huscarls" 1 0.4 3 0 "{ denmark, }"
	, R "Norman Knights" 1 0.4 3 0 "{ sicily, }"
	, R "Polish Nobles" 1 0.4 3 0 "{ poland, }"
	, R "Druzhina" 1 0.4 3 0 "{ russia, }"
	, R "Hungarian Nobles" 1 0.4 3 0 "{ hungary, }"
	, R "Byzantine Cavalry" 1 0.4 3 0 "{ byzantium, }"
	, R "Mailed Knights" 1 0.4 3 0 "{ france, hre, spain, portugal, milan, venice, papal_states, sicily, }"
	, R "Feudal Knights" 1 0.4 3 0 "{ hungary, }"
	]
	++ zip (repeat Equestrian)
	[ R "Lancers" 1 0.5 4 0 "{ france, }"
	, R "Gothic Knights" 1 0.5 4 0 "{ hre, }"
	, R "Stradiots" 1 0.5 4 0 "{ venice, }"
	, R "Polish Guard" 1 0.5 4 0 "{ poland, }"
	, R "Tsars Guard" 1 0.5 4 0 "{ russia, }"
	, R "Royal Banderium" 1 0.5 4 0 "{ hungary, }"
	, R "Kataphractoi" 1 0.5 4 0 "{ byzantium, }"
	, R "Christian Guard" 1 0.5 4 0 "{ moors, }"
	, R "Royal Mamluks" 1 0.5 4 0 "{ egypt, }"
	, R "Quapukulu" 1 0.5 4 0 "{ turks, }"
	, R "Khan's Guard" 1 0.5 4 0 "{ mongols, timurids, }"
	, R "Reiters" 1 0.5 4 0 "{ hre, }  and event_counter gunpowder_discovered 1"
	, R "English Knights" 1 0.7 6 0 "{ england, }"
	, R "Chivalric Knights" 1 0.7 6 0 "{ france, denmark, spain, sicily, }"
	, R "Imperial Knights" 1 0.7 6 0 "{ hre, }"
	, R "Portuguese Knights" 1 0.7 6 0 "{ portugal, }"
	, R "Italian MAA" 1 0.7 6 0 "{ milan, venice, papal_states, }"
	, R "Polish Knights" 1 0.7 6 0 "{ poland, }"
	, R "Dvor Cavalry" 1 0.7 6 0 "{ russia, }"
	, R "E Chivalric Knights" 1 0.7 6 0 "{ hungary, }"
	, R "Latinkon" 1 0.7 6 0 "{ byzantium, }"
	, R "Granadine Lancers" 1 0.7 6 0 "{ moors, }"
	, R "Mamluks" 1 0.7 6 0 "{ egypt, }"
	, R "Sipahi Lancers" 1 0.7 6 0 "{ turks, }"
	, R "Mongol Heavy Lancers" 1 0.7 6 0 "{ mongols, timurids, }"
	, R "Feudal Knights" 1 0.7 6 1 "{ scotland, }"
	, R "Druzhina" 1 0.7 6 0 "{ russia, }"
	, R "Byzantine Lancers" 1 0.7 6 0 "{ byzantium, }"
	, R "Granadine Jinetes" 1 0.7 6 0 "{ moors, }"
	, R "Mamluk Archers" 1 0.7 6 0 "{ egypt, }"
	, R "Sipahis" 1 0.7 6 0 "{ turks, }"
	, R "Mongol Heavy Archers" 1 0.7 6 0 "{ mongols, timurids, }"
	, R "Mailed Knights" 1 0.7 6 0 "{ scotland, }"
	, R "Boyar Sons" 1 0.7 6 0 "{ russia, }"
	, R "Byzantine Cavalry" 1 0.7 6 0 "{ byzantium, }"
	, R "Arab Cavalry" 1 0.7 6 0 "{ moors, egypt, }"
	, R "Turkomans" 1 0.7 6 0 "{ turks, timurids, }"
	, R "Mongol Light Lancers" 1 0.7 6 0 "{ mongols, }"
	, R "Hobilars" 1 0.7 6 0 "{ england, }"
	, R "Border Horse" 1 0.7 6 0 "{ scotland, }"
	, R "Mounted Sergeants" 1 0.7 6 0 "{ france, milan, venice, papal_states, sicily, }"
	, R "Scouts" 1 0.7 6 0 "{ denmark, }"
	, R "Jinetes" 1 0.7 6 0 "{ spain, portugal, }"
	, R "Polish Shooters" 1 0.7 6 0 "{ poland, }"
	, R "Kazaks" 1 0.7 6 0 "{ russia, }"
	, R "Magyar Cavalry" 1 0.7 6 0 "{ hungary, }"
	, R "Skythikon" 1 0.7 6 0 "{ byzantium, }"
	, R "Desert Cavalry" 1 0.7 6 0 "{ moors, egypt, }"
	, R "Turkish Horse Archers" 1 0.7 6 0 "{ turks, timurids, }"
	, R "Mongol Horse Archers" 1 0.7 6 0 "{ mongols, }"
	, R "Mailed Knights" 1 0.7 6 0 "{ Normans, }"
	, R "Hussars" 1 0.7 6 0 "{ poland, }"
	, R "Cossack Cavalry" 1 0.7 6 0 "{ russia, }"
	, R "Elephants" 1 0.4 3 0 "{ timurids, }"
	, R "Elephant Artillery" 1 0.4 3 0 "{ timurids, }"
	, R "Feudal Knights" 1 0.7 6 0 "{ england, france, hre, denmark, spain, portugal, milan, venice, papal_states, hungary, }"
	, R "Norman Knights" 1 0.7 6 0 "{ sicily, }"
	, R "Mailed Knights" 1 0.7 6 0 "{ england, france, hre, spain, portugal, milan, venice, papal_states, sicily, }"
	, R "Huscarls" 1 0.7 6 0 "{ denmark, }"
	, R "Polish Nobles" 1 0.7 6 0 "{ poland, }"
	, R "Hungarian Nobles" 1 0.7 6 0 "{ hungary, }"
	, R "Mounted Sergeants" 1 0.7 6 0 "{ hre, }"
	, R "Mailed Knights" 1 0.7 6 0 "{ Normans, }"
	]
	++ zip (repeat Barracks)
	[ R "Musketeers" 1 0.5 4 0 "{ spain, portugal, milan, venice, }  and event_counter gunpowder_discovered 1"
	, R "Janissary Musketeers" 1 0.5 4 0 "{ turks, }  and event_counter gunpowder_discovered 1"
	, R "Arquebusiers" 1 0.7 6 0 "{ england, france, hre, denmark, spain, milan, venice, papal_states, sicily, poland, russia, }  and event_counter gunpowder_discovered 1"
	, R "Portuguese Arquebusiers" 1 0.7 6 0 "{ portugal, }  and event_counter gunpowder_discovered 1"
	, R "Sudanese Gunners" 1 0.7 6 0 "{ moors, egypt, }  and event_counter gunpowder_discovered 1"
	, R "Janissary Archers" 1 0.7 6 0 "{ turks, }"
	, R "Pikemen" 1 0.7 6 0 "{ france, }"
	, R "Heavy Bill Militia" 1 0.7 6 0 "{ england, }"
	, R "Pike Militia" 1 0.7 6 0 "{ france, hre, spain, portugal, milan, venice, papal_states, sicily, }"
	, R "Hand Gunners" 1 0.7 6 0 "{ denmark, poland, }  and event_counter gunpowder_discovered 1"
	, R "Berdiche Axemen" 1 0.7 6 0 "{ russia, }"
	, R "Arquebusiers" 1 0.7 6 0 "{ hungary, }  and event_counter gunpowder_discovered 1"
	, R "Varangian Guard" 1 0.7 6 0 "{ byzantium, }"
	, R "ME Hand Gunners" 1 0.7 6 0 "{ moors, turks, timurids, }  and event_counter gunpowder_discovered 1"
	, R "Tabardariyya" 1 0.7 6 0 "{ egypt, }"
	, R "Hand Gunners" 1 0.7 6 0 "{ hre, spain, portugal, milan, venice, papal_states, sicily, }  and event_counter gunpowder_discovered 1"
	, R "Bill Militia" 1 0.7 6 0 "{ england, }"
	, R "Heavy Pike Militia" 1 0.7 6 0 "{ scotland, }"
	, R "Partisan Militia" 1 0.7 6 0 "{ france, }"
	, R "Halberd Militia" 1 0.7 6 0 "{ hre, papal_states, sicily, poland, hungary, }"
	, R "Swordstaff Militia" 1 0.7 6 0 "{ denmark, }"
	, R "Swordsmen Militia" 1 0.7 6 0 "{ spain, portugal, }"
	, R "Italian Cavalry Militia" 1 0.7 6 0 "{ milan, venice, }"
	, R "EE Cavalry Militia" 1 0.7 6 0 "{ russia, }"
	, R "Byzantine Infantry" 1 0.7 6 0 "{ byzantium, }"
	, R "Urban Militia" 1 0.7 6 0 "{ moors, }"
	, R "ME Halberd Militia" 1 0.7 6 0 "{ egypt, turks, timurids, }"
	, R "Archer Militia" 1 0.7 6 0 "{ england, }"
	, R "Scots Pike Militia" 1 0.7 6 0 "{ scotland, }"
	, R "Crossbow Militia" 1 0.7 6 0 "{ france, hre, denmark, spain, portugal, }"
	, R "Genoese Crossbow Militia" 1 0.7 6 0 "{ milan, }"
	, R "Pavise Crossbow Militia" 1 0.7 6 0 "{ venice, papal_states, sicily, hungary, }"
	, R "EE Crossbow Militia" 1 0.7 6 0 "{ poland, russia, }"
	, R "S Archer Militia" 1 0.7 6 0 "{ byzantium, }"
	, R "ME Crossbow Militia" 1 0.7 6 0 "{ moors, }"
	, R "Saracen Militia" 1 0.7 6 0 "{ egypt, turks, }"
	, R "Sabadar Militia" 1 0.7 6 0 "{ timurids, }"
	, R "Spear Militia" 1 0.7 6 0 "{ england, scotland, france, hre, denmark, spain, portugal, Normans, }"
	, R "Italian Spear Militia" 1 0.7 6 0 "{ milan, venice, papal_states, sicily, }"
	, R "EE Spear Militia" 1 0.7 6 0 "{ poland, russia, hungary, }"
	, R "SE Spear Militia" 1 0.7 6 0 "{ byzantium, }"
	, R "ME Spear Militia" 1 0.7 6 0 "{ moors, egypt, turks, timurids, }"
	, R "Town Militia" 1 0.7 6 0 "{ england, scotland, france, hre, denmark, spain, portugal, Normans, }"
	, R "Italian Militia" 1 0.7 6 0 "{ milan, venice, papal_states, sicily, }"
	, R "EE Town Militia" 1 0.7 6 0 "{ poland, hungary, }"
	, R "EE Archer Militia" 1 0.7 6 0 "{ russia, }"
	, R "SE Town Militia" 1 0.7 6 0 "{ byzantium, }"
	, R "ME Town Militia" 1 0.7 6 0 "{ moors, turks, timurids, }"
	, R "ME Archer Militia" 1 0.7 6 0 "{ egypt, }"
	, R "Peasant Spearmen" 1 0.7 6 0 "{ Saxons, }"
	, R "ME Spear Militia" 1 0.7 6 1 "{ mongols, }"
	, R "Spear Militia" 1 0.7 6 0 "{ Normans, }"
	, R "ME Town Militia" 1 0.7 6 0 "{ mongols, }"
	, R "Town Militia" 1 0.7 6 0 "{ Normans, }"
	, R "Peasant Spearmen" 1 0.7 6 0 "{ Saxons, }"
	]
	++ zip (repeat CastleBarracks)
	[ R "Noble Swordsmen" 1 0.7 6 0 "{ scotland, }"
	, R "Pikemen" 1 0.7 6 0 "{ france, }"
	, R "Dismounted Broken Lances" 1 0.7 6 0 "{ sicily, }"
	, R "Dismounted Christian Guard" 1 0.7 6 0 "{ moors, }"
	, R "Heavy Billmen" 1 0.7 6 0 "{ england, }"
	, R "Noble Pikemen" 1 0.7 6 0 "{ scotland, }"
	, R "Obudshaer" 1 0.7 6 0 "{ denmark, }"
	, R "Armored Swordsmen" 1 0.7 6 0 "{ england, }"
	, R "Highland Pikemen" 1 0.7 6 0 "{ scotland, }"
	, R "Voulgier" 1 0.7 6 0 "{ france, }"
	, R "Zweihander" 1 0.7 6 0 "{ hre, }"
	, R "Norse Axemen" 1 0.7 6 0 "{ denmark, }"
	, R "Sword and Buckler Men" 1 0.7 6 0 "{ spain, sicily, }"
	, R "Aventuros" 1 0.7 6 0 "{ portugal, }"
	, R "Venetian Heavy Infantry" 1 0.7 6 1 "{ venice, }"
	, R "Dismounted Druchima" 1 0.7 6 0 "{ russia, }"
	, R "Pavise Spearmen" 1 0.7 6 1 "{ hungary, }"
	, R "Dismounted Latinkon" 1 0.7 6 0 "{ byzantium, }"
	, R "Lamtuna Spearmen" 1 0.7 6 1 "{ moors, }"
	, R "Naffatun" 1 0.7 6 1 "{ egypt, turks, }"
	, R "Naffatun" 1 0.7 6 0 "{ mongols, timurids, }"
	, R "Dismounted Huscarls" 1 0.7 6 0 "{ denmark, }"
	, R "Billmen" 1 0.7 6 0 "{ england, }"
	, R "Highland Nobles" 1 0.7 6 0 "{ scotland, }"
	, R "Armored Sergeants" 1 0.7 6 1 "{ france, hre, milan, venice, papal_states, sicily, }"
	, R "Norse Swordsmen" 1 0.7 6 0 "{ denmark, }"
	, R "Almughavars" 1 0.7 6 1 "{ spain, portugal, }"
	, R "EE Spearmen" 1 0.7 6 1 "{ poland, russia, }"
	, R "Croat Axemen" 1 0.7 6 0 "{ hungary, }"
	, R "Dismounted Byzantine Lancers" 1 0.7 6 0 "{ byzantium, }"
	, R "Nubian Spearmen" 1 0.7 6 1 "{ moors, egypt, }"
	, R "Azabs" 1 0.7 6 1 "{ turks, }"
	, R "Mongol Infantry" 1 0.7 6 1 "{ mongols, }"
	, R "Afghan Javelinmen" 1 0.7 6 1 "{ timurids, }"
	, R "Armored Sergeants" 1 0.7 6 0 "{ Normans, }"
	, R "Levy Spearmen" 1 0.7 6 0 "{ england, }"
	, R "Highlanders" 1 0.7 6 0 "{ scotland, }"
	, R "Sergeant Spearmen" 1 0.7 6 1 "{ france, hre, milan, venice, papal_states, sicily, }"
	, R "Viking Raiders" 1 0.7 6 0 "{ denmark, }"
	, R "Javelinmen" 1 0.7 6 1 "{ spain, }"
	, R "Lusitanian Javelinmen" 1 0.7 6 1 "{ portugal, }"
	, R "Woodsmen" 1 0.7 6 0 "{ poland, russia, }"
	, R "Slav Levies" 1 0.7 6 0 "{ hungary, }"
	, R "Byzantine Spearmen" 1 0.7 6 3 "{ byzantium, }"
	, R "Berber Spearmen" 1 0.7 6 1 "{ moors, }"
	, R "Kurdish Javelinmen" 1 0.7 6 1 "{ egypt, }"
	, R "Turkish Javelinmen" 1 0.7 6 1 "{ turks, }"
	, R "ME Levy Spearmen" 1 0.7 6 1 "{ mongols, }"
	, R "Sergeant Spearmen" 1 0.7 6 0 "{ Normans, }"
	, R "Peasant Spearmen" 1 0.7 6 0 "{ Saxons, }"
	, R "Peasants" 1 0.7 6 0 "{ england, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, }"
	, R "Highland Rabble" 1 0.7 6 0 "{ scotland, }"
	, R "Southern Peasants" 1 0.7 6 0 "{ sicily, byzantium, }"
	, R "EE Peasants" 1 0.7 6 0 "{ poland, russia, hungary, }"
	, R "ME Peasants" 1 0.7 6 0 "{ moors, egypt, turks, mongols, timurids, }"
	]
	++ zip (repeat ProfessionalMilitary)
	[ R "Gendarmes" 1 0.7 6 0 "{ france, spain, }"
	, R "Demi Lancers" 1 0.7 6 1 "{ england, }"
	, R "French Mounted Archers" 1 0.7 6 0 "{ france, }"
	, R "Tercio Pikemen" 1 0.7 6 0 "{ spain, }"
	]
	++ zip (repeat Missiles)
	[ R "Dismounted Longbowmen" 1 0.7 6 0 "{ england, }"
	, R "Aventurier" 1 0.7 6 0 "{ france, }"
	, R "Mounted Crossbowmen" 1 0.7 6 0 "{ hre, denmark, spain, portugal, milan, venice, papal_states, sicily, }"
	, R "Granadine CB Cav" 1 0.7 6 0 "{ moors, }"
	, R "Yeoman Archers" 1 0.7 6 0 "{ england, }"
	, R "Noble Highland Archers" 1 0.7 6 0 "{ scotland, }"
	, R "Crossbowmen" 1 0.7 6 0 "{ france, denmark, hungary, }"
	, R "Pavise Crossbowmen" 1 0.7 6 0 "{ hre, spain, portugal, papal_states, sicily, }"
	, R "Genoese Crossbowmen" 1 0.7 6 0 "{ milan, }"
	, R "Venetian Archers" 1 0.7 6 0 "{ venice, }"
	, R "Lithuanian Cavalry" 1 0.7 6 0 "{ poland, }"
	, R "Byzantine Guard Archers" 1 0.7 6 0 "{ byzantium, }"
	, R "ME Peasant Crossbowmen" 1 0.7 6 0 "{ moors, }"
	, R "Nubian Archers" 1 0.7 6 1 "{ egypt, }"
	, R "Ottoman Infantry" 1 0.7 6 1 "{ turks, }"
	, R "Longbowmen" 1 0.7 6 0 "{ england, }"
	, R "Highland Archers" 1 0.7 6 0 "{ scotland, }"
	, R "Peasant Crossbowmen" 1 0.7 6 0 "{ france, hre, spain, portugal, milan, venice, papal_states, }"
	, R "Norse Archers" 1 0.7 6 0 "{ denmark, }"
	, R "Sicilian Muslim Archers" 1 0.7 6 0 "{ sicily, }"
	, R "Lithuanian Archers" 1 0.7 6 0 "{ poland, }"
	, R "Bosnian Archers" 1 0.7 6 0 "{ hungary, }"
	, R "Trebizond Archers" 1 0.7 6 0 "{ byzantium, }"
	, R "Desert Archers" 1 0.7 6 0 "{ moors, egypt, }"
	, R "Turkish Archers" 1 0.7 6 0 "{ turks, timurids, }"
	, R "Mongol Foot Archers" 1 0.7 6 0 "{ mongols, }"
	, R "Peasant Archers" 1 0.7 6 0 "{ england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, Normans, Saxons, }"
	, R "S Peasant Archers" 1 0.7 6 0 "{ sicily, hungary, byzantium, }"
	, R "EE Peasant Archers" 1 0.7 6 0 "{ poland, }"
	, R "EE Peasant Archers" 1 0.7 6 1 "{ russia, }"
	, R "Sudanese Javelinmen" 1 0.7 6 0 "{ moors, }"
	, R "ME Peasant Archers" 1 0.7 6 0 "{ egypt, turks, mongols, timurids, }"
	]
	++ zip (repeat Siege) siegeRecruits
	++ zip (repeat CastleSiege) siegeRecruits
	++ zip (repeat Cannon) cannonRecruits
	++ zip (repeat CastleCannon) cannonRecruits
	++ zip (repeat UrbanEquestrian)
	[ R "Arab Cavalry" 1 0.5 4 0 "{ moors, egypt, }"
	, R "Turkomans" 1 0.5 4 0 "{ turks, timurids, }"
	, R "Mongol Light Lancers" 1 0.5 4 0 "{ mongols, }"
	, R "Granadine Jinetes" 1 0.4 3 0 "{ moors, }"
	, R "Mamluk Archers" 1 0.4 3 0 "{ egypt, }"
	, R "Sipahis" 1 0.4 3 0 "{ turks, }"
	, R "Mongol Heavy Archers" 1 0.4 3 0 "{ mongols, timurids, }"
	]
	++ zip (repeat Port) portRecruits
	++ zip (repeat CastlePort) portRecruits
	++ zip (repeat Tavern)
	[ R "Transilvanian Peasants" 1 0.5 4 0 "{ hungary, }"
	]
	++ zip (repeat CityHall)
	[ R "Forlorn Hope" 1 0.4 3 0 "{ hre, }"
	, R "Carroccio Standard M" 1 0.4 3 0 "{ milan, }"
	, R "Carroccio Standard V" 1 0.4 3 0 "{ venice, }"
	, R "Famiglia Ducale" 1 0.5 4 0 "{ milan, }"
	, R "Janissary Heavy Inf" 1 0.5 4 0 "{ turks, }"
	]
	++ zip (repeat Bullring)
	[ R "Jinetes" 1 0.5 4 0 "{ spain, portugal, }"
	]
	++ zip (repeat Caravan)
	[ R "Tuareg Camel Spearmens" 1 0.5 4 0 "{ moors, }"
	, R "Camel Gunners" 1 0.4 3 0 "{ moors, }  and event_counter gunpowder_discovered 1"
	]
	++ zip (repeat AssassinsGuild)
	[ R "Battlefield Assassins" 1 0.4 3 1 "{ hungary, }"
	]
	++ zip (repeat AssassinsMuslimGuild)
	[ R "Hashishim" 1 0.4 3 2 "{ moors, egypt, turks, }"
	]
-- Note: there is some ugly, hacky (though harmless) code in the vanilla M2TW; we can't just copy
-- the guild HQ's units over here b/c that would leave out some other units, like Sowrdsmen Militia
	++ zip (repeat MasonsGuild)
	[ R "Berdiche Axemen" 1 0.5 4 0 "{ russia, }"
	, R "Bill Militia" 1 0.5 4 0 "{ england, }"
	, R "Byzantine Infantry" 1 0.5 4 0 "{ byzantium, }"
	, R "EE Spear Militia" 1 0.5 4 0 "{ russia, }"
	, R "Halberd Militia" 1 0.5 4 0 "{ hre, papal_states, sicily, poland, hungary, }"
	, R "Heavy Bill Militia" 1 0.5 4 0 "{ england, }"
	, R "Heavy Pike Militia" 1 0.5 4 0 "{ scotland, }"
	, R "Italian Spear Militia" 1 0.5 4 0 "{ milan, venice, }"
	, R "ME Halberd Militia" 1 0.5 4 0 "{ egypt, turks, timurids, }"
	, R "ME Spear Militia" 1 0.5 4 0 "{ mongols, }"
	, R "Partisan Militia" 1 0.5 4 0 "{ france, }"
	, R "Pike Militia" 1 0.5 4 0 "{ france, hre, spain, portugal, milan, venice, papal_states, sicily, }"
	, R "Swordsmen Militia" 1 0.5 4 0 "{ spain, portugal, }"
	, R "Swordstaff Militia" 1 0.5 4 0 "{ denmark, }"
	, R "Urban Militia" 1 0.5 4 0 "{ moors, }"
	]
	++ zip (repeat MerchantsGuild)
-- Give +1 XP for England's Merchant Cavalry Militia as well, just like everyone else. (Bug in M2TW
-- 1.3).
	[ R "Merchant Cavalry Militia" 1 0.5 4 1 "{ england, scotland, france, hre, denmark, spain, portugal, poland, hungary, }"
	, R "Italian Cavalry Militia" 1 0.5 4 1 "{ milan, venice, papal_states, sicily, }"
	, R "EE Cavalry Militia" 1 0.5 4 1 "{ russia, }"
	, R "Greek Militia Cavalry" 1 0.5 4 1 "{ byzantium, }"
	, R "Arab Cavalry" 1 0.5 4 1 "{ moors, egypt, }"
	, R "Sipahis" 1 0.5 4 1 "{ turks, }"
	]
	++ zip (repeat TemplarsChapterHouse)
	[ R"Knights Templar" 1 0.7 6 2 "{ england, scotland, france, denmark, milan, venice, papal_states, sicily, poland, hungary, }"
	]
	++ zip (repeat StJohnsChapterHouse)
	[ R"Knights Hospitaller" 1 0.7 6 2 "{ england, scotland, france, denmark, milan, venice, papal_states, sicily, poland, hungary, }"
	]
	++ zip (repeat TeutonicKnightsChapterHouse)
	[ R"Teutonic Knights" 1 0.7 6 2 "{ hre, }"
	]
	++ zip (repeat KnightsOfSantiagoChapterHouse)
	[ R"Knights of Santiago" 1 0.7 6 2 "{ spain, portugal, }"
	]
	++ zip (repeat WoodsmensGuild)
	[ R"Sherwood Archers" 1 0.4 3 0 "{ england, }"
	]
	where
	siegeRecruits =
		[ R "NE Trebuchet" 1 0.4 3 0 "{ england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, }"
		, R "EE Trebuchet" 1 0.4 3 0 "{ poland, russia, hungary, }"
		, R "GR Trebuchet" 1 0.4 3 0 "{ byzantium, }"
		, R "ME Trebuchet" 1 0.4 3 0 "{ moors, egypt, turks, }"
		, R "AS Trebuchet" 1 0.4 3 0 "{ mongols, timurids, }"
		, R "NE Catapult" 1 0.4 3 0 "{ england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, Normans, }"
		, R "EE Catapult" 1 0.4 3 0 "{ poland, russia, hungary, }"
		, R "GR Catapult" 1 0.4 3 0 "{ byzantium, }"
		, R "ME Catapult" 1 0.4 3 0 "{ moors, egypt, turks, }"
		, R "AS Catapult" 1 0.4 3 0 "{ mongols, timurids, }"
		, R "NE Ballista" 1 0.4 3 0 "{ england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, Normans, }"
		, R "EE Ballista" 1 0.4 3 0 "{ poland, russia, hungary, }"
		, R "GR Ballista" 1 0.4 3 0 "{ byzantium, }"
		, R "ME Ballista" 1 0.4 3 0 "{ moors, egypt, turks, }"
		, R "AS Ballista" 1 0.4 3 0 "{ mongols, timurids, }"
		]
	cannonRecruits =
		[ R "NE Basilisk" 1 0.4 3 0 "{ france, hre, spain, portugal, }"
		, R "NE Monster Ribault" 1 0.4 3 0 "{ milan, venice, }"
		, R "EE Basilisk" 1 0.4 3 0 "{ russia, hungary, }"
		, R "ME Monster Bombard" 1 0.4 3 0 "{ turks, }"
		, R "NE Culverin" 1 0.4 3 0 "{ england, scotland, france, spain, portugal, milan, venice, papal_states, }"
		, R "NE Cannon" 1 0.4 3 0 "{ hre, sicily, }"
		, R "NE Serpentine" 1 0.4 3 0 "{ denmark, }"
		, R "EE Serpentine" 1 0.4 3 0 "{ poland, hungary, }"
		, R "EE Cannon" 1 0.4 3 0 "{ russia, }"
		, R "ME Cannon" 1 0.4 3 0 "{ moors, egypt, turks, }"
		, R "AS Cannon" 1 0.4 3 0 "{ timurids, }"
		, R "NE Serpentine" 1 0.4 3 0 "{ france, hre, }"
		, R "NE Cannon" 1 0.4 3 0 "{ denmark, }"
		, R "EE Cannon" 1 0.4 3 0 "{ poland, }"
		, R "NE Mortar" 1 0.4 3 0 "{ england, scotland, venice, papal_states, sicily, }"
		, R "NE Grand Bombard" 1 0.4 3 0 "{ france, hre, spain, portugal, milan, }"
		, R "NE Ribault" 1 0.4 3 0 "{ denmark, }"
		, R "EE Ribault" 1 0.4 3 0 "{ poland, hungary, }"
		, R "EE Grand Bombard" 1 0.4 3 0 "{ russia, }"
		, R "ME Grand Bombard" 1 0.4 3 0 "{ moors, egypt, turks, }"
		, R "AS Grand Bombard" 1 0.4 3 0 "{ timurids, }"
		, R "NE Ribault" 1 0.4 3 0 "{ england, scotland, spain, portugal, milan, venice, papal_states, sicily, }"
		, R "NE Bombard" 1 0.4 3 0 "{ england, scotland, france, hre, denmark, spain, portugal, milan, venice, papal_states, sicily, }"
		, R "EE Bombard" 1 0.4 3 0 "{ poland, russia, hungary, }"
		, R "GR Bombard" 1 0.4 3 0 "{ byzantium, }"
		, R "ME Bombard" 1 0.4 3 0 "{ moors, egypt, turks, }"
		, R "AS Rocket Launcher" 1 0.4 3 0 "{ mongols, }"
		, R "AS Bombard" 1 0.4 3 0 "{ timurids, }"
		]
	portRecruits =
		[ R "cog" 1 1 3 0 "{ hre, france, scotland, england, }"
		, R "longboat" 1 1 3 0 "{ denmark, }"
		, R "holk" 1 1 3 0 "{ hre, scotland, france, england, }"
		, R "dragon boat" 1 1 3 0 "{ denmark, }"
		, R "gun holk" 1 1 3 0 "{ northern_european, }"
		, R "carrack" 1 1 3 0 "{ northern_european, } and event_counter world_is_round 1"
		, R "dhow" 1 1 3 0 "{ middle_eastern, }"
		, R "war galley" 1 1 3 0 "{ egypt, moors, turks, }"
		, R "lanternas" 1 1 3 0 "{ egypt, moors, turks, }"
		, R "baghlah" 1 1 3 0 "{ egypt, moors, turks, } and event_counter world_is_round 1"
		, R "cog" 1 1 3 0 "{ hungary, poland, }"
		, R "ladya" 1 1 3 0 "{ russia, }"
		, R "war galley" 1 1 3 0 "{ hungary, }"
		, R "holk" 1 1 3 0 "{ russia, poland, }"
		, R "gun holk" 1 1 3 0 "{ russia, poland, }"
		, R "lanternas" 1 1 3 0 "{ hungary, }"
		, R "carrack" 1 1 3 0 "{ eastern_european, } and event_counter world_is_round 1"
		, R "dromon" 1 1 3 0 "{ greek, }"
		, R "fire ship" 1 1 3 0 "{ greek, }"
		, R "lanternas" 1 1 3 0 "{ greek, }"
		, R "carrack" 1 1 3 0 "{ greek, } and event_counter world_is_round 1"
		, R "galley" 1 1 3 0 "{ sicily, papal_states, milan, venice, }"
		, R "cog" 1 1 3 0 "{ portugal, spain, }"
		, R "war galley" 1 1 3 0 "{ southern_european, }"
		, R "lanternas" 1 1 3 0 "{ portugal, spain, papal_states, milan, sicily, }"
		, R "caravel" 1 1 3 0 "{ portugal, spain, } and event_counter world_is_round 1"
		, R "galleass" 1 1 3 0 "{ venice, }"
		, R "carrack" 1 1 3 0 "{ papal_states, sicily, milan, venice, } and event_counter world_is_round 1"
		, R "grande carrack" 1 1 3 0 "{ portugal, spain, } and event_counter world_is_round 1"
		]
