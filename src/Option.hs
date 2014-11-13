{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Option where

import System.Console.CmdArgs.Implicit

import Data hiding (name)
import Error
import Meta

data Opts = Opts
	{ game :: Game
	, installed_data_dir :: FilePath
	, unpacked_data_dir :: FilePath
	, anims_binary_dir :: FilePath
	, bdiff_dir :: FilePath
	, no_check :: Bool
	, no_sha :: Bool
	, out :: FilePath
	} deriving (Data, Typeable, Show, Eq)

qsOpts :: Opts
qsOpts = Opts
	{ game = RTW &= help "game type; either \"M2TW\" or \"RTW\" (default RTW)"
	, installed_data_dir = def &= typDir
		&= help'
			["path to the `data' directory"
			, "inside your RTW/M2TW installation folder"
			]
	, unpacked_data_dir = def &= typDir
		&= help'
			[ "(M2TW only) path to the `data' directory"
			, "created by the official M2TW unpacker.exe tool"
			]

	, anims_binary_dir = def &= typDir
		&= help "(M2TW only) path to the vanilla (packed) animations files"
	, bdiff_dir = def &= typDir
		&= help
"(M2TW only) path to the .bdiff files; in qs's source control, the path is\
\ `src/bdiff'"
	, no_check = def
		&= help'
			["disable both file existence checks"
			, "and SHA-1 checks on source files"
			]
	, no_sha = def
		&= help'
			["disable SHA-1 checks on source"
			, "files (existence checking still performed)"
			]
	, out = "./" &= typDir
		&= help' ["path to output the mod; default is the current directory"]
	}
	where
	help' = help . unwords

getOpts :: IO Opts
getOpts = cmdArgs $ qsOpts
	&= verbosityArgs [explicit, name "Verbose", name "V"] []
	&= versionArg [explicit, name "version", name "v", summary _QS_INFO]
	&= summary (_QS_INFO ++ ", " ++ _COPYRIGHT)
	&= help "quicksilver RTW/M2TW mod generator"
	&= helpArg [explicit, name "help", name "h"]
	&= program _QS_NAME

-- Basic option sanity check.
checkOpts :: Opts -> IO ()
checkOpts Opts{..}
	| (game == M2TW && null unpacked_data_dir)
		= abort ("--unpacked-data-dir is empty", 1)
	| (game == M2TW && null anims_binary_dir)
		= abort ("--anims-binary-dir is empty", 1)
	| (game == M2TW && null bdiff_dir)
		= abort ("--bdiff-dir is empty", 1)
	| (null installed_data_dir) = abort ("--installed-data-dir is empty", 1)
	| otherwise = return ()
