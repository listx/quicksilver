{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BC
import IO
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

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
    createDirectoryIfMissing True $ _MOD_PATH_DATA ++ "animations"
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
                \[game]\n"
                ++ "event_cutscenes = 0\n" -- disable forced combat closeups during battle, when an enemy general is killed, a gate is broken down, or a wall is destroyed
                ++ "unlimited_men_on_battlefield = 1\n" -- disable graphics cap during battle (always allow reinforcements regardless of M2TW's opinion of user's hardware)

                ++ "\n\
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
    | elem fpath _TO_EDIT   = editFile parentDir fpath >>
                              diffFile parentDir fpath
    | elem fpath _TO_DIFF   = applyBinaryDiff parentDir fpath
    | otherwise             = copyFile' parentDir fpath

applyBinaryDiff :: FilePath -> FilePath -> IO ()
applyBinaryDiff parentDir fpath
    | fpath == _PDAT    = bdiff parentDir fpath _PDAT'
    | fpath == _PIDX    = bdiff parentDir fpath _PIDX'
    | fpath == _SDAT    = bdiff parentDir fpath _SDAT'
    | fpath == _SIDX    = bdiff parentDir fpath _SIDX'
    | otherwise = return ()
    where
        bdiff :: FilePath -> FilePath -> FilePath -> IO ()
        bdiff parentDir fpath delta = do
            putStr $ "Resolving deltas for " ++ enquote dest ++ "... "
            (sin, sout, serr, p) <- createProcess diffCmd
            waitForProcess p
            putStrLn "done"
            where
                sourcePath = parentDir ++ "/" ++ fpath
                dest = _MOD_PATH_DATA ++ fpath
                diffCmd = CreateProcess
                    { cmdspec = ShellCommand ("xdelta3 -d -s " ++ dquote sourcePath ++ " " ++ dquote delta ++ " " ++ dquote dest)
                    , cwd = Nothing
                    , env = Nothing
                    , std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = Inherit
                    , close_fds = False
                    }

diffFile :: FilePath -> FilePath -> IO ()
diffFile parentDir fpath = do
    putStr $ "Writing diff " ++ enquote patchDest ++ "... "
    (sin, sout, serr, p) <- createProcess diffCmd
    diffData <- case sout of
        Just h -> hGetContents h
        Nothing -> return ""
    writeFile patchDest diffData
    waitForProcess p
    putStrLn "done"
    where
        sourcePath = parentDir ++ "/" ++ fpath
        dest = _MOD_PATH_DATA ++ fpath
        fname = snd $ splitFileName fpath
        patchDest = _GEN_PATH ++ fname ++ ".patch"
        diffCmd = CreateProcess
            { cmdspec = ShellCommand ("diff -uN " ++ dquote sourcePath ++ " " ++ dquote dest)
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }

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
            | fpath == _DCD     = transform     _DCD_FUNCS
            | fpath == _DCL     = transform     _DCL_FUNCS
            | fpath == _DFS     = transform     _DFS_FUNCS
            | fpath == _DS      = transform     _DS_FUNCS
            | fpath == _DM      = transform     _DM_FUNCS
            | fpath == _DSK     = transform     _DSK_FUNCS
            | fpath == _DSM     = transform     _DSM_FUNCS
            | fpath == _DSR     = transform     _DSR_FUNCS
            | fpath == _DW      = transform     _DW_FUNCS
            | fpath == _EDB     = transform'    _EDB_FUNCS  "^\\}\\r\\n"    "}\r\n"
            | fpath == _EDCT    = transform     _EDCT_FUNCS
            | fpath == _EDU     = transform'    _EDU_FUNCS  " \\r\\n \\r\\n"    " \r\n \r\n"
            | otherwise = id

transform :: [[(String, BC.ByteString -> BC.ByteString, BC.ByteString -> Bool)]] -> BC.ByteString -> BC.ByteString
transform funcs = compose (map grpGsub funcs)

-- First split up the source string into subparts, then apply the regex transformations for each
-- part.
transform' :: [[(String, BC.ByteString -> BC.ByteString, BC.ByteString -> Bool)]] -> String -> String -> BC.ByteString -> BC.ByteString
transform' funcs delim delim' src = BC.intercalate (BC.pack delim') (map applyRegexes parts)
    where
        parts = splitBy (makeRegexDef $ BC.pack delim) src
        applyRegexes = compose (map grpGsub funcs)
