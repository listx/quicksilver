{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BC
import IO
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

import Check
import Error
import Option
import Regex
import Source
import Util

_DC     = "descr_character.txt"
_DCL    = "descr_cultures.txt"
_DFS    = "descr_faction_standing.txt"
_DP     = "descr_projectile.txt"
_DS     = "world/maps/campaign/imperial_campaign/descr_strat.txt"
_DW     = "descr_walls.txt"
_EDB    = "export_descr_buildings.txt"
_EDCT   = "export_descr_character_traits.txt"
_EDU    = "export_descr_unit.txt"

_TO_EDIT =
    [ _DC
    , _DCL
    , _DFS
    , _DS
    , _DW
    , _EDB
    , _EDCT
    , _EDU
    ]

_GEN_PATH       = "gen/"
_MOD_PATH_DATA  = _GEN_PATH ++ _QS_NAME ++ "/data/"
_MOD_PATH_TOP   = _GEN_PATH

data SourceParent =
      IFolder
    | Unpacked
    deriving (Eq, Show)

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ putStrLn "Use -h or --help." >> exitWith ExitSuccess
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    opts <- getOpts
    putStrLn $ "Data path: `" ++ installed_data_dir opts ++ "'"
    putStrLn $ "Unpacked data path: `" ++ unpacked_data_dir opts ++ "'"
    checkOpts opts
    qs opts

qs :: Opts -> IO ()
qs opts@Opts{..} = do
    putStrLn "\nStarting mod generation... "
    createGenDir
    createBat
    createCfg
    mapM_ (modSource opts udd) _SHA1_UDD_SOURCES
    mapM_ (modSource opts idd) _SHA1_IDD_SOURCES
    putStrLn $ "\nquicksilver mod version " ++ _QS_VERSION ++ " successfully generated."
    where
        udd = unpacked_data_dir
        idd = installed_data_dir

createGenDir :: IO ()
createGenDir = do
    genExist <- doesDirectoryExist _GEN_PATH
    when genExist $ abort ("`" ++ _GEN_PATH ++ "' already exists", 1)
    putStr "Ensuring that directory `./gen' exists... "
    createDirectoryIfMissing True _MOD_PATH_DATA
    putStrLn "OK"

createBat :: IO ()
createBat = do
    putStr "Writing quicksilver.bat... "
    writeFile (_MOD_PATH_TOP ++ "quicksilver.bat") contents
    putStrLn "done"
    where
        contents = "medieval2.exe @quicksilver.cfg"

createCfg :: IO ()
createCfg = do
    putStr "Writing quicksilver.cfg... "
    writeFile (_MOD_PATH_TOP ++ "quicksilver.cfg") contents
    putStrLn "done"
    where
        contents =  "[features]\n\
                    \mod = quicksilver\n\
                    \\n\
                    \[log]\n\
                    \to = logs/quicksilver.log.txt\n\
                    \level = * error\n\
                    \\n\
                    \[game]\n\
                    \event_cutscenes = 0\n\
                    \\n\
                    \[misc]\n\
                    \unlock_campaign = true"

modSource :: Opts -> FilePath -> (Integer, FilePath) -> IO ()
modSource opts@Opts{..} = f
    where
        f :: FilePath -> (Integer, FilePath) -> IO ()
        f parentDir (cksum, fpath)
            | elem fpath _TO_EDIT   = editFile  opts cksum parentDir fpath
            | otherwise             = copyFile' opts cksum parentDir fpath
            where
                fname = snd $ splitFileName fpath
                _DC_PATH   = udd ++ "/" ++ _DC
                _DCL_PATH  = udd ++ "/" ++ _DCL
                _DFS_PATH  = udd ++ "/" ++ _DFS
                _DP_PATH   = udd ++ "/" ++ _DP
                _DS_PATH   = idd ++ "/" ++ _DS
                _DW_PATH   = udd ++ "/" ++ _DW
                _EDB_PATH  = udd ++ "/" ++ _EDB
                _EDCT_PATH = udd ++ "/" ++ _EDCT
                _EDU_PATH  = udd ++ "/" ++ _EDU
                udd = unpacked_data_dir
                idd = installed_data_dir

copyFile' :: Opts -> Integer -> FilePath -> FilePath -> IO ()
copyFile' Opts{..} cksum parentDir fpath = do
    createDirectoryIfMissing True modSubdir
    when (not no_sha) $ checkSum fullpath cksum
    putStr $ "Copying file `" ++ fname ++ "'... "
    copyFile fullpath $ _MOD_PATH_DATA ++ fpath
    putStrLn "done"
    where
        modSubdir = _MOD_PATH_DATA ++ fst (splitFileName fpath)
        fname = snd $ splitFileName fpath
        fullpath = parentDir ++ "/" ++ fpath

editFile :: Opts -> Integer -> FilePath -> FilePath -> IO ()
editFile Opts{..} cksum parentDir fpath = do
    -- check that source files exist
    checkExists sourcePath
    -- check SHA1 sums
    when (not no_sha) $ checkSum sourcePath cksum
    src <- BC.readFile sourcePath
    -- apply one (or more) transformations on the source file
    putStr $ "Writing new `" ++ fpath ++ "'... "
    BC.writeFile (_MOD_PATH_DATA ++ fpath) $ transformFile src
    putStrLn "done"
    where
        fname = snd $ splitFileName fpath
        sourcePath = parentDir ++ "/" ++ fpath
        transformFile :: BC.ByteString -> BC.ByteString
        transformFile
            | fpath == _DC      = transform     _DC_FUNCS
            | fpath == _DCL     = transform     _DCL_FUNCS
            | fpath == _DFS     = transform     _DFS_FUNCS
            | fpath == _DS      = transform     _DS_FUNCS
            | fpath == _DW      = transform     _DW_FUNCS
            | fpath == _EDB     = transform'    _EDB_FUNCS "^\\}\\r\\n"        "}\r\n"
            | fpath == _EDCT    = transform     _EDCT_FUNCS
            | fpath == _EDU     = transform'    _EDU_FUNCS " \\r\\n \\r\\n"    " \r\n \r\n"
            | otherwise = \src -> src

transform :: [[(String, BC.ByteString -> BC.ByteString, BC.ByteString -> Bool)]] -> BC.ByteString -> BC.ByteString
transform funcs = compose (map grpGsub funcs)

-- First split up the source string into subparts, then apply the regex transformations for each
-- part.
transform' :: [[(String, BC.ByteString -> BC.ByteString, BC.ByteString -> Bool)]] -> String -> String -> BC.ByteString -> BC.ByteString
transform' funcs delim delim' src = BC.intercalate (BC.pack delim') (map applyRegexes parts)
    where
        parts = splitBy (makeRegexDef $ BC.pack delim) src
        applyRegexes = compose (map grpGsub funcs)
