module Data.RTW where

import Data
import Meta
import Regex.RTW

qsRTW :: Mod
qsRTW = Mod RTW "quicksilverRTW" _QS_VERSION  miscFilesRTW $
	-- Text files to mod
	map (\f -> makeModFileModText f Installed) rtwInstalledModText
	++
	-- Misc files to copy over
	map (\f -> makeModFile f Installed Copy) rtwInstalledCopy

miscFilesRTW :: [(FilePath, String)]
miscFilesRTW =
	[ ("quicksilverRTW.bat", bat)
	]
	where
	bat = "RomeTW.exe "
		++ "-mod:quicksilverRTW "
		++ "-enable_editor " -- enable the hisorical battle editor (clickable link in game menu)
		++ "-show_err " -- show fatal error messages (after a crash)
		++ "-nm " -- disable intro/background movies

rtwInstalledModText :: [(Integer, String, Operation)]
rtwInstalledModText =
	[ (0xebc3c3b4ee4813921ed2265ddf29366a55327c5a,
		_RTW_DC  , ModText _RTW_DC_FUNCS   ("", ""))
	, (0x1ce28a66802f22271d493bccc0c8d5f6d1ce53da,
		_RTW_DCL , ModText _RTW_DCL_FUNCS  ("", ""))
	, (0xa7722e54af17e0902f9060042c50202745d673d4,
		_RTW_DMR , ModText _RTW_DMR_FUNCS  ("", ""))
	, (0x38629eb7c9dc5ea12782da9a945819057f3e34b6,
		_RTW_DR,   ModText _RTW_DR_FUNCS   ("", ""))
	, (0x5403a058bd49d371ae6cca739b0656043a833181,
		_RTW_DS  , ModText _RTW_DS_FUNCS   ("", ""))
	, (0x360fdc7e95c0f4e457fb7a150c788b30a22dda67,
		_RTW_DSN , ModText _RTW_DSN_FUNCS  ("", ""))
	, (0x29aba3632af603dc25d5f94d46257cbf0185935e,
		_RTW_DSR , ModText _RTW_DSR_FUNCS  ("", ""))
	, (0x5ea7c9c9f381d0d019655e9bf3ee253970eb58a6,
		_RTW_EDB , ModText _RTW_EDB_FUNCS  ("^\\}\\r\\n", "}\r\n"))
	, (0x1dd045b7f252d06f8c2c0fa27a18a6ef802b3422,
		_RTW_EDCT, ModText _RTW_EDCT_FUNCS ("", ""))
	, (0x55b56dc9ac99161de270c28f5efed2c791d318ca,
		_RTW_EDU , ModText _RTW_EDU_FUNCS  (" \\r\\n \\r\\n", " \r\n \r\n"))
	, (0xb52ae2c0bce88e1fedb01a0fee600e335238572f,
		_RTW_L,    ModText _RTW_L_FUNCS    ("", ""))
	]

rtwInstalledCopy :: [(Integer, String)]
rtwInstalledCopy =
	[ (0x2a0ac0fdfa1da2e50a1dc41a5b3f67b45705e391,
		"world/maps/base/descr_disasters.txt")
	, (0x38629eb7c9dc5ea12782da9a945819057f3e34b6,
		"world/maps/base/descr_regions.txt")
	, (0x202b6d079f0412acccdd47b86d12b90cc9a69a72,
		"world/maps/base/descr_terrain.txt")
	, (0xc359676a0f9fd41274fb58402ad3b6dee2c36807,
		"world/maps/base/map_climates.tga")
	, (0xe1739d99e0b84dc5a8f16b9fdcf20b76483734e8,
		"world/maps/base/map_features.tga")
	, (0xd43efd8461a3fc1f67431c6b17f81680dc75b433,
		"world/maps/base/map_FE.tga")
	, (0xe1ebbc473491cf90cb3ac35f8c9881e2be1580a6,
		"world/maps/base/map_ground_types.tga")
	, (0xd5cdc742ff7b52fdf261fb2317747fdac0814cf3,
		"world/maps/base/map_heights.hgt")
	, (0x60d5fb09524f59ccb08cf9c5d473038e1ccf7c67,
		"world/maps/base/map_heights.tga")
	, (0xcc90a59261ddcfc76299762b76f8a4302d86cd1e,
		"world/maps/base/map_regions.tga")
	, (0x9437c120bcd4c603f3c86dcd31bd583464c45fa7,
		"world/maps/base/map_roughness.tga")
	, (0x9dadfc81c38a850ae5e8ee0fee825860c2fdc795,
		"world/maps/base/map_trade_routes.tga")
	, (0x8243bb8dcee8a69fde163ee1804b369a59d97394,
		"world/maps/base/water_surface.tga")
	, (0x195876362cb7f806e243d70a75f198df96001ba3,
		"world/maps/campaign/imperial_campaign/descr_events.txt")
	, (0xd9ecb2925190769979f9e94f14fb3f3430107b70,
		"world/maps/campaign/imperial_campaign/description_romans_brutii.txt")
	, (0xc0596c7a1342fbb259c87e48dfd91b265987b1e9,
		"world/maps/campaign/imperial_campaign/description_romans_julii.txt")
	, (0x122754e8a2036046e9a8d0bd976a01833157b3ef,
		"world/maps/campaign/imperial_campaign/description_romans_scipii.txt")
	, (0x82ceefd14e4b8d7346808c7992bbdd5305f9e1df,
		"world/maps/campaign/imperial_campaign/description.txt")
	, (0x5085a2ab63043d046614163278d60ce656f01528,
		concat
			[ "world/maps/campaign/imperial_campaign/"
			, "descr_regions_and_settlement_name_lookup.txt"]
			)
	, (0xa644f9ed2e56f0de504112cdc28419b4b25f554c,
		"world/maps/campaign/imperial_campaign/descr_win_conditions.txt")
	, (0x3f7167403a6a7b4d19125600fe830aa5197dd3dd,
		"world/maps/campaign/imperial_campaign/disasters.tga")
	, (0x8560b7003018083f21a9f1c364e083e980a3de83,
		"world/maps/campaign/imperial_campaign/leader_pic_romans_brutii.tga")
	, (0x2afd4e0730634b23cc5e43714556d5469c1e7f33,
		"world/maps/campaign/imperial_campaign/leader_pic_romans_julii.tga")
	, (0xfc7b5e77c186c35c67a9f0ab173e2ba5dc2c1836,
		"world/maps/campaign/imperial_campaign/leader_pic_romans_scipii.tga")
	, (0xfef28260a2dabeeb180cad467c271fa6212fdf37,
		"world/maps/campaign/imperial_campaign/map_britons.tga")
	, (0x0e08d40de99cc811057620c2fe18398efa274800,
		"world/maps/campaign/imperial_campaign/map_carthage.tga")
	, (0x176ac89ca438446fbbd086f1ee5a92a35bb08e05,
		"world/maps/campaign/imperial_campaign/map_egypt.tga")
	, (0x38ecdc7c29c0f675e9e813ed50fad7a80cdc1fd3,
		"world/maps/campaign/imperial_campaign/map_gauls.tga")
	, (0xf77dc83bb895b68fe65b01cb879b3738c7c28ace,
		"world/maps/campaign/imperial_campaign/map_germans.tga")
	, (0x2395e9d18f1fa270cc0f015d68e0ab6588f16353,
		"world/maps/campaign/imperial_campaign/map_greek_cities.tga")
	, (0x194964614d549e739c69aa1e63509dad6b58adc6,
		"world/maps/campaign/imperial_campaign/map_parthia.tga")
	, (0xecf78cac95cb1dc89402ea4ed88339087c7a7f0c,
		"world/maps/campaign/imperial_campaign/map_romans_brutii.tga")
	, (0x16ac872b5cc7d1273676db5da8f11c784123ff76,
		"world/maps/campaign/imperial_campaign/map_romans_julii.tga")
	, (0x7d186640439a809e53cfc1b118d98645b672328a,
		"world/maps/campaign/imperial_campaign/map_romans_scipii.tga")
	, (0x84491732e0b10cbde8c1eef3314f0a72a27d2459,
		"world/maps/campaign/imperial_campaign/map_seleucid.tga")
	, (0xefd1a518c90e08966b1419935e63f9b8a4509cd9,
		"world/maps/campaign/imperial_campaign/radar_map1.tga")
	, (0x8f12a334767ebb244ee9f1efeaba1e44de266c50,
		"world/maps/campaign/imperial_campaign/radar_map2.tga")
	, (0xb56e4ea8d373492591e59f059b144338b88f9a8d,
		"text/imperial_campaign_regions_and_settlement_names.txt")
	]

_RTW_DC  :: String
_RTW_DCL :: String
_RTW_DMR :: String
_RTW_DR  :: String
_RTW_DS  :: String
_RTW_DSN :: String
_RTW_DSR :: String
_RTW_EDB :: String
_RTW_EDCT :: String
_RTW_EDSA :: String
_RTW_EDU :: String
_RTW_L :: String

_RTW_DC     = "descr_character.txt"
_RTW_DCL    = "descr_cultures.txt"
_RTW_DMR    = "world/maps/campaign/imperial_campaign/descr_mercenaries.txt"
_RTW_DR     = "world/maps/base/descr_regions.txt"
_RTW_DS     = "world/maps/campaign/imperial_campaign/descr_strat.txt"
_RTW_DSN    = "descr_senate.txt"
_RTW_DSR    = "descr_sm_resources.txt"
_RTW_EDB    = "export_descr_buildings.txt"
_RTW_EDCT   = "export_descr_character_traits.txt"
_RTW_EDSA   = "export_descr_sounds_advice.txt"
_RTW_EDU    = "export_descr_unit.txt"
_RTW_L      = "text/landmarks.txt"
