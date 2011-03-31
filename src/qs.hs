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
    putStrLn $ "Data path: " ++ (enquote $ installed_data_dir opts)
    putStrLn $ "Unpacked data path: " ++ (enquote $ unpacked_data_dir opts)
    checkOpts opts
    qs opts

qs :: Opts -> IO ()
qs opts@Opts{..} = do
    putStrLn "\nStarting mod generation... "
    createGenDir
    createFile "quicksilver.bat" bat
    createFile "quicksilver.cfg" cfg
    putStr "\n"
    mapM_ (checkFile opts udd) _SHA1_UDD_SOURCES
    mapM_ (checkFile opts idd) _SHA1_IDD_SOURCES
    putStr "\n"
    mapM_ (modSource udd) _SHA1_UDD_SOURCES
    mapM_ (modSource idd) _SHA1_IDD_SOURCES
    putStrLn $ "\nquicksilver version " ++ _QS_VERSION ++ " successfully generated inside " ++ enquote _GEN_PATH
    where
        udd = unpacked_data_dir
        idd = installed_data_dir
        bat =   "medieval2.exe @quicksilver.cfg"
        cfg =   "[features]\n\
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

createGenDir :: IO ()
createGenDir = do
    genExist <- doesDirectoryExist _GEN_PATH
    when genExist $ abort (enquote _GEN_PATH ++ " already exists", 1)
    putStr $ "Ensuring that directory " ++ enquote _GEN_PATH ++ " exists... "
    createDirectoryIfMissing True _MOD_PATH_DATA
    putStrLn "OK"

createFile :: String -> String -> IO ()
createFile fname contents = do
    putStr $ "Writing " ++ enquote dest ++ "... "
    writeFile dest contents
    putStrLn "done"
    where
        dest = _MOD_PATH_TOP ++ fname

modSource :: FilePath -> (Integer, FilePath) -> IO ()
modSource parentDir (_, fpath)
    | elem fpath _TO_EDIT   = editFile parentDir fpath
    | otherwise             = copyFile' parentDir fpath

copyFile' :: FilePath -> FilePath -> IO ()
copyFile' parentDir fpath = do
    createDirectoryIfMissing True modSubdir
    putStr $ "Copying file " ++ enquote sourcePath ++ "... "
    copyFile sourcePath dest
    putStrLn "done"
    where
        dest = _MOD_PATH_DATA ++ fpath
        modSubdir = _MOD_PATH_DATA ++ fst (splitFileName fpath)
        fname = snd $ splitFileName fpath
        sourcePath = parentDir ++ "/" ++ fpath

editFile :: FilePath -> FilePath -> IO ()
editFile parentDir fpath = do
    src <- BC.readFile sourcePath
    -- apply one (or more) transformations on the source file
    putStr $ "Writing new " ++ enquote dest ++ "... "
    BC.writeFile dest $ transformFile src
    putStrLn "done"
    where
        dest = _MOD_PATH_DATA ++ fpath
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
