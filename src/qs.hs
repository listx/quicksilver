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

getModDataPath :: Game -> String
getModDataPath g = getGenPath g ++ _QS_NAME ++ "/data/"

getGenPath :: Game -> String
getGenPath g = case g of
    RTW -> "genRTW/"
    _ -> "genM2TW/"

data SourceParent =
      IFolder
    | Unpacked
    deriving (Eq, Show)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else Prelude.id) getOpts
    putStrLn $ "Data path: " ++ (enquote $ installed_data_dir opts)
    putStrLn $ "Unpacked data path: " ++ (enquote $ unpacked_data_dir opts)
    checkOpts opts
    qs opts

qs :: Opts -> IO ()
qs opts@Opts{..} = do
    putStrLn "\nStarting mod generation... "
    createGenDir gameType
    when (gameType == M2TW) $ createDirectoryIfMissing True $ getModDataPath gameType ++ "animations"
    createLaunchScripts
    putStr "\n"
    mapM_ (checkFile opts idd udd) (snd (if gameType == RTW then qsRTW else qsM2TW))
    putStr "\n"
    mapM_ (modSource gameType idd udd) (snd (if gameType == RTW then qsRTW else qsM2TW))
    putStrLn $ "\nquicksilver version " ++ _QS_VERSION ++ " successfully generated inside " ++ enquote (getGenPath gameType)
    where
        udd = unpacked_data_dir
        idd = installed_data_dir
        createLaunchScripts = if gameType == RTW
            then createFile gameType "quicksilver.bat" batRTW
            else do createFile gameType "quicksilver.bat" batM2TW
                    createFile gameType "quicksilver.cfg" cfgM2TW
        batRTW = "RomeTW.exe "
            ++ "-mod:quicksilver "
            ++ "-enable_editor " -- enable the hisorical battle editor (clickable link in game menu)
            ++ "-show_err " -- show fatal error messages (after a crash)
            ++ "-nm " -- disable intro/background movies
        batM2TW = "medieval2.exe @quicksilver.cfg"
        cfgM2TW = "[features]\n\
            \mod = quicksilver\n\
            \\n\
            \[log]\n\
            \to = logs/quicksilver.log.txt\n\
            \level = * error\n\
            \\n\
            \[game]\n"
            -- disable forced combat closeups during battle, when an enemy
            -- general is killed, a gate is broken down, or a wall is destroyed
            ++ "event_cutscenes = 0\n"
            -- disable graphics cap during battle (always allow reinforcements
            -- regardless of M2TW's opinion of user's hardware)
            ++ "unlimited_men_on_battlefield = 1\n"

            ++ "\n\
            \[misc]\n\
            \unlock_campaign = true"
        gameType = if game == M2TW
            then fst qsM2TW
            else fst qsRTW

createGenDir :: Game -> IO ()
createGenDir game = do
    genExist <- doesDirectoryExist gp
    when genExist $ abort (enquote gp ++ " already exists", 1)
    putStr $ "Ensuring that directory " ++ enquote gp ++ " exists... "
    createDirectoryIfMissing True (getModDataPath game)
    putStrLn "OK"
    where
        gp = getGenPath game

createFile :: Game -> String -> String -> IO ()
createFile game fname contents = do
    putStr $ "Writing " ++ enquote dest ++ "... "
    writeFile dest contents
    putStrLn "done"
    where
        dest = getGenPath game ++ fname

modSource :: Game -> FilePath -> FilePath -> ModFile -> IO ()
modSource game idd udd mf@ModFile{..} = case operation of
    ModText _ _ -> editFile game parentDir mf >> diffFile game parentDir mf
    ModBinary _ -> applyBinaryDiff game parentDir mf
    _ -> copyFile' game parentDir mf
    where
        parentDir = if origin == Installed
            then idd
            else udd

applyBinaryDiff :: Game -> FilePath -> ModFile -> IO ()
applyBinaryDiff game parentDir ModFile{..} = do
    putStr $ "Resolving deltas for " ++ enquote dest ++ "... "
    (_, _, _, p) <- createProcess diffCmd
    _ <- waitForProcess p
    putStrLn "done"
    where
        sourcePath = parentDir ++ "/" ++ name
        dest = (getModDataPath game) ++ name
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
diffFile :: Game -> FilePath -> ModFile -> IO ()
diffFile game parentDir ModFile{..} = do
    putStr $ "Writing diff " ++ enquote patchDest ++ "... "
    (_, sout, _, p) <- createProcess diffCmd
    diffData <- case sout of
        Just h -> hGetContents h
        Nothing -> return ""
    writeFile patchDest diffData
    _ <- waitForProcess p
    putStrLn "done"
    where
        sourcePath = parentDir ++ "/" ++ name
        dest = (getModDataPath game) ++ name
        fname = snd $ splitFileName name
        patchDest = getGenPath game ++ fname ++ ".patch"
        diffCmd = CreateProcess
            { cmdspec = ShellCommand ("diff -uN " ++ dquote sourcePath ++ " " ++ dquote dest)
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }

copyFile' :: Game -> FilePath -> ModFile -> IO ()
copyFile' game parentDir ModFile{..} = do
    createDirectoryIfMissing True modSubdir
    putStr $ "Copying file " ++ enquote sourcePath ++ "... "
    copyFile sourcePath dest
    putStrLn "done"
    where
        dest = mpd ++ name
        modSubdir = mpd ++ fst (splitFileName name)
        sourcePath = parentDir ++ "/" ++ name
        mpd = getModDataPath game

editFile :: Game -> FilePath -> ModFile -> IO ()
editFile game parentDir mf@ModFile{..} = do
    src <- BC.readFile sourcePath
    -- apply one (or more) transformations on the source file
    putStr $ "Writing new " ++ enquote dest ++ "... "
    when (elem '/' name) $ createDirectoryIfMissing True (takeDirectory dest)
    BC.writeFile dest $ transform mf src
    putStrLn "done"
    where
        mpd = getModDataPath game
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
