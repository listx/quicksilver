{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Option where

import System.Console.CmdArgs.Implicit

import Data hiding (name)
import Error

_QS_NAME, _QS_VERSION, _QS_INFO, _COPYRIGHT :: String
_QS_NAME = "quicksilver"
_QS_VERSION = "0.1.0.0"
_QS_INFO = _QS_NAME ++ " version " ++ _QS_VERSION
_COPYRIGHT = "(C) Linus Arver 2011"

data Opts = Opts
	{ game :: Game
	, installed_data_dir :: FilePath
	, unpacked_data_dir :: FilePath
	, anims_binary_dir :: FilePath
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
	| (null unpacked_data_dir && game == M2TW) = abort
		("--unpacked-data-path is empty", 1)
	| (null installed_data_dir) = abort ("--data-path is empty", 1)
	| otherwise = return ()
