module Data.M2TW where

import Data
import Regex.M2TW

qsM2TW :: Mod
qsM2TW = (M2TW,
    -- Text files to mod
    map (\f -> makeModFileModText f Unpacked) m2twUnpackedModText
    ++
    [ ModFile 0x852dd3c9a4a6647f38af2f84eb07874de152acf8 _M2TW_DS Installed
		(ModText _M2TW_DS_FUNCS ("^\\r\\n\\r\\n", "\r\n\r\n"))]
    ++
    -- Binary files to modify (with binary diffs)
    [ ModFile 0xdb22c7400f27ce863dd7c92201eea6183ba30335 _M2TW_PDAT Installed (ModBinary _M2TW_PDAT)
    , ModFile 0x044a8a79011589ba340a3bffc87642a02bc87c7a _M2TW_PIDX Installed (ModBinary _M2TW_PIDX)
    , ModFile 0x47deb00beb5bdb2d2bfd4aa7bb8d1452ed6747b5 _M2TW_SDAT Installed (ModBinary _M2TW_SDAT)
    , ModFile 0x70b656a6831c1f2da80eb04448011472a69865fc _M2TW_SIDX Installed (ModBinary _M2TW_SIDX)
    ]
    ++
    -- Misc files to copy over
    map (\f -> makeModFile f Installed Copy) m2twInstalledCopy
    )

m2twUnpackedModText :: [(Integer, String, Operation)]
m2twUnpackedModText =
    [ (0x9505d63491e7c90debba66016f4a837173fa8374, _M2TW_DC  , ModText _M2TW_DC_FUNCS   ("", ""))
    , (0xe9896f7ab02de7d308b590b2ebbbc2918fe375c8, _M2TW_DCD , ModText _M2TW_DCD_FUNCS  ("", ""))
    , (0x3dbe5d41d1a0754a63e9673ccc42070c0ea61ac6, _M2TW_DCAD, ModText _M2TW_DCAD_FUNCS ("", ""))
    , (0x0d121a3b5567f1d326abde8f7f270c8fe6469952, _M2TW_DCL , ModText _M2TW_DCL_FUNCS  ("", ""))
    , (0x6e9bc8a4ec4e938ba5124e049079758efe8e2ed2, _M2TW_DFS , ModText _M2TW_DFS_FUNCS  ("", ""))
    , (0xe1016813ec6e0f1f621d01c73c6b1e9465777bc3, _M2TW_DM  , ModText _M2TW_DM_FUNCS   ("", ""))
    , (0x40e74a92a91c4ac10e6d798b0de25664cab45ea1, _M2TW_DSF , ModText _M2TW_DSF_FUNCS  ("", ""))
    , (0x97e3caf49d8ceb7e09192fa18e28eb278a0f56fe, _M2TW_DSK , ModText _M2TW_DSK_FUNCS  ("", ""))
    , (0x25a82cb8d15146a1c58f7b1cfeb09c7bb2fb5953, _M2TW_DSM , ModText _M2TW_DSM_FUNCS  ("", ""))
    , (0x5783e8565f7d43acf9054b833272bebca5423998, _M2TW_DSR , ModText _M2TW_DSR_FUNCS  ("", ""))
    , (0x959d554513c313b12be95ac9579bff3b4aa49521, _M2TW_DW  , ModText _M2TW_DW_FUNCS   ("", ""))
    , (0xb604c776df95aaa51e9fae9bf3b1ccbf34df8138, _M2TW_EDA , ModText _M2TW_EDA_FUNCS  ("", ""))
    , (0xc106543e9a05abe82bd56b245674e9e7c14a3d81, _M2TW_EDAN, ModText _M2TW_EDAN_FUNCS ("", ""))
    , (0xca2018a694fbabcaf5a058f4e87889acc3d35f89, _M2TW_EDB , ModText _M2TW_EDB_FUNCS  ("^\\}\\r\\n", "}\r\n"))
    , (0x2c1095e9bb17d078d4719e57cc7bf52a959ef6a9, _M2TW_EDBE, ModText _M2TW_EDBE_FUNCS ("", ""))
    , (0x2a95300126b5f78294caffa0af8658f4576a2d40, _M2TW_EDCT, ModText _M2TW_EDCT_FUNCS ("", ""))
    , (0x2c533185c43f1c025cf5817111437c19fc993117, _M2TW_EDG,  ModText _M2TW_EDG_FUNCS  ("", ""))
    , (0x140f93465e48577a5262d6e104630518426a7a13, _M2TW_EDU,  ModText _M2TW_EDU_FUNCS  (" \\r\\n \\r\\n", " \r\n \r\n"))
    ]

m2twInstalledCopy :: [(Integer, String)]
m2twInstalledCopy =
    [ (0x82c4a8ce1d5c474b379ca7eed4273f969ebe6f10, "sounds/events.dat")
    , (0x2ea5c502f5366dec134cc8671bcce2bd7c82b9bc, "sounds/events.idx")
    , (0xc444b41da651a0e6499c047d17eec5e74bc10065, "world/maps/base/descr_disasters.txt")
    , (0x52566bb7ea9e26791fc0697db8c76483e3130234, "world/maps/base/descr_regions.txt")
    , (0x0eb5595000d0bdcedb4026a1e09dc31ac2c1e28d, "world/maps/base/descr_sounds_music_types.txt")
    , (0xc51e6d8b57b7a2352c474a218bad0237c446ad86, "world/maps/base/descr_terrain.txt")
    , (0x6b21c2c2a854ed111eb9ee0623304d7384ce6aa8, "world/maps/base/map_climates.tga")
    , (0xe06f734e8eed7e9eeadffe54cfa38587084e4f62, "world/maps/base/map_features.tga")
    , (0x68d1e3897ca6a28c5b0a11848061f5542c9e37a0, "world/maps/base/map_FE.tga")
    , (0x8fdbbc638bec5784c725758a5a4fe6ac8ef411a5, "world/maps/base/map_fog.tga")
    , (0x6634b18c1ac15daa3628aa60d3cf46ed334f83ae, "world/maps/base/map_ground_types.tga")
    , (0xacaeb46f474671820217d9104adee43399c92306, "world/maps/base/map_heights.hgt")
    , (0x2c6464782469f91af68a3f20a7e92a265bf74942, "world/maps/base/map_heights.tga")
    , (0xb7f231818adcbd4d2473a56b5c1e674e2f037e25, "world/maps/base/map_regions.tga")
    , (0x70166f79cd6444b637b684254362bbb76a4b94cc, "world/maps/base/map_roughness.tga")
    , (0xc1ad57da6d3aca5dbf3cd20d27f8b03d3827c94c, "world/maps/base/map.rwm")
    , (0x9df759c170840a82bdef971c3ab6ed2fe2cfed9e, "world/maps/base/map_trade_routes.tga")
    , (0x58f5b608c35966b92113f00ea286af7ca2268137, "world/maps/base/water_surface.tga")
    , (0x474bdb07a5d234780c54ec4aa49bf64ec9938153, "world/maps/campaign/imperial_campaign/campaign_script.txt")
    , (0xaf575b0d63d1e61037fd57b14b129f8a8659bc23, "world/maps/campaign/imperial_campaign/descr_events.txt")
    , (0xe5102d9ce3fcdc5a059b7070ab388a5ac6b6a853, "world/maps/campaign/imperial_campaign/description_milan.txt")
    , (0xf4de215f076dfe5d0704212fe5696469536a2fdf, "world/maps/campaign/imperial_campaign/description_sicily.txt")
    , (0x82ceefd14e4b8d7346808c7992bbdd5305f9e1df, "world/maps/campaign/imperial_campaign/description.txt")
    , (0xf6fa1461772edde8da9ba98e7dd7423bd1c96962, "world/maps/campaign/imperial_campaign/description_venice.txt")
    , (0xa2fe4ed7369472728c6480bea8597cd8bdcab84f, "world/maps/campaign/imperial_campaign/descr_mercenaries.txt")
    , (0x451c4fe24a4cd3739aecb3c4e8e7a8dbbed40788, "world/maps/campaign/imperial_campaign/descr_regions_and_settlement_name_lookup.txt")
    , (0x7b760c91ac51d9832de2d06e9d270964ce4fe352, "world/maps/campaign/imperial_campaign/descr_win_conditions.txt")
    , (0x3f7167403a6a7b4d19125600fe830aa5197dd3dd, "world/maps/campaign/imperial_campaign/disasters.tga")
    , (0x0e08d40de99cc811057620c2fe18398efa274800, "world/maps/campaign/imperial_campaign/map_aztecs.tga")
    , (0x92951939ec1e3f0a928a044dc1db5ca14d9bfd07, "world/maps/campaign/imperial_campaign/map_byzantium.tga")
    , (0xf9bc344420ba36951da1f1b003ca26c239e8f0f8, "world/maps/campaign/imperial_campaign/map_denmark.tga")
    , (0x3b5c52475a56bab6f67a4f1fac2deff91daaf3c7, "world/maps/campaign/imperial_campaign/map_egypt.tga")
    , (0x04abe551bd467a42396d71f907b3d3efca04aa4b, "world/maps/campaign/imperial_campaign/map_england.tga")
    , (0xc9d524b1f70a4e33ef50f8fea8f3a85646423349, "world/maps/campaign/imperial_campaign/map_FE.psd")
    , (0x71ff82ad4b5154afa8113f31ce424afe05ab8566, "world/maps/campaign/imperial_campaign/map_france.tga")
    , (0xb876a382ac4ad15ad41dbf833a2422c93a03ba5e, "world/maps/campaign/imperial_campaign/map_hre.tga")
    , (0x9848f4dd1a3ad49308dc609dbe7a4bf5275c407d, "world/maps/campaign/imperial_campaign/map_hungary.tga")
    , (0x2648e094ed271a4d0fe48eb80a02991ee30a1cfb, "world/maps/campaign/imperial_campaign/map_milan.tga")
    , (0x194964614d549e739c69aa1e63509dad6b58adc6, "world/maps/campaign/imperial_campaign/map_mongols.tga")
    , (0x3875f37b07235602e47c4a30a49dd9c53dea16a1, "world/maps/campaign/imperial_campaign/map_moors.tga")
    , (0x1aadb4eeb56dc230444f63f2819b5ecad5e35fb8, "world/maps/campaign/imperial_campaign/map_papal_states.tga")
    , (0x60e6c23841ff5147e7f0b340e6f0e0a8fa3db425, "world/maps/campaign/imperial_campaign/map_poland.tga")
    , (0xb09285770e918164b1e3c3f4517b3d99acf12397, "world/maps/campaign/imperial_campaign/map_portugal.tga")
    , (0xb63b60a1a9bec3868ed9b0dffd90c7b5b5f694e7, "world/maps/campaign/imperial_campaign/map_russia.tga")
    , (0x14eea8f36707fd2c36e45facd20fdf4c4f719fa7, "world/maps/campaign/imperial_campaign/map_scotland.tga")
    , (0xa8a2a57e0c94e23394efe4f6c9eff40402594868, "world/maps/campaign/imperial_campaign/map_sicily.tga")
    , (0xbb61baf9de2639b626fb598cac5be63e8abbdec7, "world/maps/campaign/imperial_campaign/map_spain.tga")
    , (0xb35cb2b80e74b50b5543603f5dc4cba55255fb35, "world/maps/campaign/imperial_campaign/map_turks.tga")
    , (0x0c83daa7d00daf9aa6387c786054c18eecf3f0c9, "world/maps/campaign/imperial_campaign/map_venice.tga")
    , (0xb21580837fdb838c040d0a75b79c637fb504d1d7, "world/maps/campaign/imperial_campaign/radar_map1.tga")
    , (0x35ff31c70473de547b31d8456d823e75621e3380, "world/maps/campaign/imperial_campaign/radar_map2.tga")
    , (0x40d15e747ac1f0c46c5722b374438efdaa0e26ea, "world/maps/campaign/imperial_campaign/vc_byzantium.tga")
    , (0xfa7cc39df35d0accc0fac325cd27609479a8e35b, "world/maps/campaign/imperial_campaign/vc_denmark.tga")
    , (0x2858924f2a29302afb67098df78225a9e0ca443a, "world/maps/campaign/imperial_campaign/vc_egypt.tga")
    , (0x5bb9ec5788634fa2f618e857d26c6fa19fb9fa71, "world/maps/campaign/imperial_campaign/vc_england.tga")
    , (0xfa7cc39df35d0accc0fac325cd27609479a8e35b, "world/maps/campaign/imperial_campaign/vc_france.tga")
    , (0x11238a1be570b3c3a4883bafb9e828263c3e33a0, "world/maps/campaign/imperial_campaign/vc_hre.tga")
    , (0x7816d5570d4b0d2f0185fd12c32bbb49ff2c1479, "world/maps/campaign/imperial_campaign/vc_hungary.tga")
    , (0x5824721c46e82594d58f82490626329c48d96960, "world/maps/campaign/imperial_campaign/vc_milan.tga")
    , (0x08686c3df2597634f001208ca3033624860ad91e, "world/maps/campaign/imperial_campaign/vc_moors.tga")
    , (0xb8f7d40140161d91cae49444f7bd7b93170a6c8e, "world/maps/campaign/imperial_campaign/vc_poland.tga")
    , (0x230e3d4fba6555d3479d0f0364e744934b7ff271, "world/maps/campaign/imperial_campaign/vc_portugal.tga")
    , (0xa5a7c1952fbb1c8ef68d5b5612d3679720b5458b, "world/maps/campaign/imperial_campaign/vc_russia.tga")
    , (0xbdccba6a4c5a6b75bc639816088570aed61c4046, "world/maps/campaign/imperial_campaign/vcs_byzantium.tga")
    , (0x7816d5570d4b0d2f0185fd12c32bbb49ff2c1479, "world/maps/campaign/imperial_campaign/vc_scotland.tga")
    , (0x502b605fafc752c571ea149ce28fed3dde088b4a, "world/maps/campaign/imperial_campaign/vcs_denmark.tga")
    , (0x8cfc390baf901cb1612ba22fa8386e8f43ebeebe, "world/maps/campaign/imperial_campaign/vcs_egypt.tga")
    , (0x8a419e3ddd7e75c3dfbc2dc2f818c65cc7d0f880, "world/maps/campaign/imperial_campaign/vcs_england.tga")
    , (0xa29a450d87369b5cf0bb5e18acf492c2e5cf95e5, "world/maps/campaign/imperial_campaign/vcs_france.tga")
    , (0x1b58cbedf37fbd18018f1c207ee6df6b5117db1b, "world/maps/campaign/imperial_campaign/vcs_hre.tga")
    , (0x40d604c60eff5711328ed8b98a75aaa401db616e, "world/maps/campaign/imperial_campaign/vcs_hungary.tga")
    , (0x8d281f12c82476606e68a685179e86e16edc2dbe, "world/maps/campaign/imperial_campaign/vc_sicily.tga")
    , (0x6a665a0548228b5c8bd14f99430394c1b23ca5e1, "world/maps/campaign/imperial_campaign/vcs_milan.tga")
    , (0x10cb59c4618d68bed14634ebbc786cbb0011da59, "world/maps/campaign/imperial_campaign/vcs_moors.tga")
    , (0x1060edebd811191fff53d1796976af7ea9e7f649, "world/maps/campaign/imperial_campaign/vc_spain.tga")
    , (0x41cc31dc1900724039a17e6cceaea6e800ef7c78, "world/maps/campaign/imperial_campaign/vcs_poland.tga")
    , (0xd9d0d2d9c2a2867f14cb3a76d860ca45b4832511, "world/maps/campaign/imperial_campaign/vcs_portugal.tga")
    , (0xe68c851b40825bfbe93e24379b01298ef5757d71, "world/maps/campaign/imperial_campaign/vcs_russia.tga")
    , (0xa29a450d87369b5cf0bb5e18acf492c2e5cf95e5, "world/maps/campaign/imperial_campaign/vcs_scotland.tga")
    , (0x9a4ea5f1b2d8364b59b262f3801c46a7369eb81a, "world/maps/campaign/imperial_campaign/vcs_sicily.tga")
    , (0x45d09cb2530898fbef5874ea59b3fc090fafb99c, "world/maps/campaign/imperial_campaign/vcs_spain.tga")
    , (0x41305c8e5d87a23680f88dc7f02b6cedaec9c33b, "world/maps/campaign/imperial_campaign/vcs_turks.tga")
    , (0xdca88c0dbbf9f19aaec32b2aaf390cf8020f7a87, "world/maps/campaign/imperial_campaign/vcs_venice.tga")
    , (0x8aea23c1fa7ffdfe9251f84ffc4ed50f571c0901, "world/maps/campaign/imperial_campaign/vc_turks.tga")
    , (0x94a6b833cc41daec7a320bd5d7b59cbd6e0fe30f, "world/maps/campaign/imperial_campaign/vc_venice.tga")
    ]

-- Short names for some common game files
_M2TW_DC :: String
_M2TW_DCD :: String
_M2TW_DCAD :: String
_M2TW_DCL :: String
_M2TW_DFS :: String
_M2TW_DM :: String
_M2TW_DS :: String
_M2TW_DSF :: String
_M2TW_DSK :: String
_M2TW_DSM :: String
_M2TW_DSR :: String
_M2TW_DW :: String
_M2TW_EDA :: String
_M2TW_EDAN :: String
_M2TW_EDB :: String
_M2TW_EDBE :: String
_M2TW_EDCT :: String
_M2TW_EDG :: String
_M2TW_EDU :: String
-- binary files
_M2TW_PDAT :: String
_M2TW_PIDX :: String
_M2TW_SDAT :: String
_M2TW_SIDX :: String
-- .bdiff files
_M2TW_PDAT' :: String
_M2TW_PIDX' :: String
_M2TW_SDAT' :: String
_M2TW_SIDX' :: String

-- Text files
_M2TW_DC     = "descr_character.txt"
_M2TW_DCD    = "descr_campaign_db.xml"
_M2TW_DCAD   = "descr_campaign_ai_db.xml"
_M2TW_DCL    = "descr_cultures.txt"
_M2TW_DFS    = "descr_faction_standing.txt"
_M2TW_DM     = "descr_missions.txt"
_M2TW_DSF    = "descr_sm_factions.txt"
_M2TW_DSK    = "descr_skeleton.txt"
_M2TW_DSM    = "descr_settlement_mechanics.xml"
_M2TW_DSR    = "descr_sm_resources.txt"
_M2TW_DW     = "descr_walls.txt"
_M2TW_EDA    = "export_descr_advice.txt"
_M2TW_EDAN   = "export_descr_ancillaries.txt"
_M2TW_EDB    = "export_descr_buildings.txt"
_M2TW_EDBE   = "export_descr_buildings_enums.txt"
_M2TW_EDCT   = "export_descr_character_traits.txt"
_M2TW_EDG    = "export_descr_guilds.txt"
_M2TW_EDU    = "export_descr_unit.txt"
_M2TW_DS     = "world/maps/campaign/imperial_campaign/descr_strat.txt"
-- Binary files
_M2TW_PDAT   = "animations/pack.dat"
_M2TW_PIDX   = "animations/pack.idx"
_M2TW_SDAT   = "animations/skeletons.dat"
_M2TW_SIDX   = "animations/skeletons.idx"
-- Binary diff sources (should be packaged with qs)
_M2TW_PDAT'  = "bdiff/pack.dat.bdiff"
_M2TW_PIDX'  = "bdiff/pack.idx.bdiff"
_M2TW_SDAT'  = "bdiff/skeletons.dat.bdiff"
_M2TW_SIDX'  = "bdiff/skeletons.idx.bdiff"
