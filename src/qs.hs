{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BC
import IO
import System.Directory
import System.Environment
import System.FilePath
import System.Process

import Check
import Error
import Option
import Data
import Data.RTW
import Data.M2TW
import Regex
import Util

getModDataPath :: FilePath -> String -> String
getModDataPath fpath mname = getGenPath fpath mname ++ mname ++ "/data/"

getGenPath :: FilePath -> String -> String
getGenPath fpath mname = fpath' ++ mname ++ "/"
    where
        fpath' = case last fpath of
            '/' -> fpath
            _ -> fpath ++ ['/']

data SourceParent =
      IFolder
    | Unpacked
    deriving (Eq, Show)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    args <- getArgs
    opts@Opts{..} <- (if null args then withArgs ["--help"] else Prelude.id) getOpts
    when (not $ null installed_data_dir) $ putStrLn $ "Data path: " ++ (enquote installed_data_dir)
    when (not $ null unpacked_data_dir) $ putStrLn $ "Unpacked data path: " ++ (enquote unpacked_data_dir)
    checkOpts opts
    qs opts

qs :: Opts -> IO ()
qs opts@Opts{..} = do
    putStrLn "\nStarting mod generation... "
    createGenDir opts
    when (game == M2TW) $ createDirectoryIfMissing True $ getModDataPath out (modName pickMod) ++ "animations"
    mapM_ (checkFile opts) modfiles
    putStr "\n"
    mapM_ (modSource opts) modfiles
    putStr "\n"
    mapM_ (\(a, b) -> createFile opts a b) miscfiles
    createReadme opts pickMod
    putStrLn $ "\nquicksilver version " ++ _QS_VERSION ++ " successfully generated inside " ++ enquote (getGenPath out (modName pickMod))
    where
        pickMod = if game == RTW then qsRTW else qsM2TW
        modfiles = modFiles pickMod
        miscfiles = miscFiles pickMod

createReadme :: Opts -> Mod -> IO ()
createReadme Opts{..} Mod{..} = do
    putStr $ "\nWriting " ++ fname ++ "... "
    writeFile dest contents
    putStrLn "done"
    where
        contents = header ++ unlines readme
        header = "#+TITLE: README for " ++ modName ++ " " ++ modVer ++ "\n\n"
        dest = (getGenPath out modName) ++ fname
        fname = modName ++ "-README.org"

createGenDir :: Opts -> IO ()
createGenDir Opts{..} = do
    genExist <- doesDirectoryExist gp
    when genExist $ abort (enquote gp ++ " already exists", 1)
    putStr $ "Ensuring that directory " ++ enquote gp ++ " exists... "
    createDirectoryIfMissing True (getModDataPath out (modName pickMod))
    putStrLn "OK"
    where
        pickMod = if game == RTW then qsRTW else qsM2TW
        gp = getGenPath out (modName pickMod)

createFile :: Opts -> String -> String -> IO ()
createFile Opts{..} fname contents = do
    putStr $ "Writing " ++ enquote dest ++ "... "
    writeFile dest contents
    putStrLn "done"
    where
        pickMod = if game == RTW then qsRTW else qsM2TW
        dest = getGenPath out (modName pickMod) ++ fname

modSource :: Opts -> ModFile -> IO ()
modSource opts@Opts{..} mf@ModFile{..} = case operation of
    ModText _ _ -> editFile opts parentDir mf >> diffFile opts parentDir mf
    ModBinary _ -> applyBinaryDiff opts parentDir mf
    _ -> copyFile' opts parentDir mf
    where
        parentDir = if origin == Installed
            then installed_data_dir
            else unpacked_data_dir

applyBinaryDiff :: Opts -> FilePath -> ModFile -> IO ()
applyBinaryDiff Opts{..} parentDir ModFile{..} = do
    putStr $ "Resolving deltas for " ++ enquote dest ++ "... "
    (_, _, _, p) <- createProcess diffCmd
    _ <- waitForProcess p
    putStrLn "done"
    where
        pickMod = if game == RTW then qsRTW else qsM2TW
        sourcePath = parentDir ++ "/" ++ name
        dest = (getModDataPath out (modName pickMod)) ++ name
        diffCmd = CreateProcess
            { cmdspec = ShellCommand ("xdelta3 -d -s " ++ dquote sourcePath ++ " " ++ dquote (bdiff operation) ++ " " ++ dquote dest)
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }

-- After modifying the text, generate a diff of it.
diffFile :: Opts -> FilePath -> ModFile -> IO ()
diffFile Opts{..} parentDir ModFile{..} = do
    putStr $ "Writing diff " ++ enquote patchDest ++ "... "
    (_, sout, _, p) <- createProcess diffCmd
    diffData <- case sout of
        Just h -> hGetContents h
        Nothing -> return ""
    writeFile patchDest diffData
    _ <- waitForProcess p
    putStrLn "done"
    where
        pickMod = if game == RTW then qsRTW else qsM2TW
        sourcePath = parentDir ++ "/" ++ name
        dest = (getModDataPath out (modName pickMod)) ++ name
        fname = snd $ splitFileName name
        patchDest = getGenPath out (modName pickMod) ++ fname ++ ".patch"
        diffCmd = CreateProcess
            { cmdspec = ShellCommand ("diff -uN " ++ dquote sourcePath ++ " " ++ dquote dest)
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }

copyFile' :: Opts -> FilePath -> ModFile -> IO ()
copyFile' Opts{..} parentDir ModFile{..} = do
    createDirectoryIfMissing True modSubdir
    putStr $ "Copying file " ++ enquote sourcePath ++ "... "
    copyFile sourcePath dest
    putStrLn "done"
    where
        pickMod = if game == RTW then qsRTW else qsM2TW
        dest = mpd ++ name
        modSubdir = mpd ++ fst (splitFileName name)
        sourcePath = parentDir ++ "/" ++ name
        mpd = getModDataPath out (modName pickMod)

editFile :: Opts -> FilePath -> ModFile -> IO ()
editFile Opts{..} parentDir mf@ModFile{..} = do
    src <- BC.readFile sourcePath
    -- apply one (or more) transformations on the source file
    putStr $ "Writing new " ++ enquote dest ++ "... "
    when (elem '/' name) $ createDirectoryIfMissing True (takeDirectory dest)
    BC.writeFile dest $ transform mf src
    putStrLn "done"
    where
        pickMod = if game == RTW then qsRTW else qsM2TW
        mpd = getModDataPath out (modName pickMod)
        dest = mpd ++ name
        sourcePath = parentDir ++ "/" ++ name

transform :: ModFile -> BC.ByteString -> BC.ByteString
transform ModFile{..} src
    | null . fst . partition $ operation = compose (map grpGsub (regexes operation)) $ src
    -- First split up the source string into subparts, then apply the regex
    -- transformations for each part.
    | otherwise = BC.intercalate (BC.pack (snd . partition $ operation)) (map applyRegexes parts)
    where
        parts = splitBy (makeRegexDef $ BC.pack (fst . partition $ operation)) src
        applyRegexes = compose (map grpGsub (regexes operation))
