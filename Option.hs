{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Option where

import Control.Monad (when)
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Verbosity

import Error

_QS_NAME = "quicksilver"
_QS_VERSION = "0.04"
_QS_INFO = _QS_NAME ++ " version " ++ _QS_VERSION
_COPYRIGHT = "(C) Linus Arver 2011"

data Opts = Opts
    { unpacked_data_dir    :: FilePath
    , installed_data_dir             :: FilePath
    , no_sha                :: Bool
    } deriving (Data, Typeable, Show, Eq)

qsOpts :: Opts
qsOpts = Opts
    { unpacked_data_dir    = def &= typDir &= help "path to the `data' directory created by the official M2TW unpacker.exe tool"
    , installed_data_dir             = def &= typDir &= help "path to the `data' directory inside your M2TW installation folder"
    , no_sha                = def &= help "disable SHA-1 checks on source files"
    }

getOpts :: IO Opts
getOpts = cmdArgs $ qsOpts
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _QS_INFO]
    &= summary (_QS_INFO ++ ", " ++ _COPYRIGHT)
    &= help "quicksilver M2TW mod generator"
    &= helpArg [explicit, name "help", name "h"]
    &= program _QS_NAME

-- Basic option sanity check, as well as ensuring that source files are the official originals.
checkOpts :: Opts -> IO ()
checkOpts Opts{..} = do
    when (null unpacked_data_dir) $ abort ("--unpacked-data-path is empty", 1)
    when (null installed_data_dir) $ abort ("--data-path is empty", 1)
