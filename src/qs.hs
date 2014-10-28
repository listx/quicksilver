{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Data.Word

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
	opts@Opts{..} <- (if null args then withArgs ["--help"] else Prelude.id)
		getOpts
	when (not $ null installed_data_dir)
		. putStrLn
		$ "Data path: " ++ (enquote installed_data_dir)
	when (not $ null unpacked_data_dir)
		. putStrLn
		$ "Unpacked data path: " ++ (enquote unpacked_data_dir)
	checkOpts opts
	qs opts

qs :: Opts -> IO ()
qs opts@Opts{..} = do
	putStrLn "\nStarting mod generation... "
	createGenDir opts
	when (game == M2TW)
		. createDirectoryIfMissing True
		$ getModDataPath out (modName pickMod) ++ "animations"
	mapM_ (checkFile opts) modfiles
	putStr "\n"
	mapM_ (modSource opts) modfiles
	putStr "\n"
	mapM_ (\(a, b) -> createFile opts a b) miscfiles
	putStrLn $ "\n" ++ modinfo ++ " successfully generated inside "
		++ enquote (getGenPath out (modName pickMod))
	where
	pickMod = if game == RTW then qsRTW else qsM2TW
	modfiles = modFiles pickMod
	miscfiles = miscFiles pickMod
	modinfo = modName pickMod ++ " version " ++ modVer pickMod

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
		{ cmdspec = ShellCommand $ concat
			[ "xdelta3 -d -s "
			, dquote sourcePath
			, " "
			, dquote (bdiff operation)
			, " "
			, dquote dest
			]
		, cwd = Nothing
		, env = Nothing
		, std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		, close_fds = False
		, create_group = False
		, delegate_ctlc = False
		}

-- After modifying the text, generate a diff of it.
diffFile :: Opts -> FilePath -> ModFile -> IO ()
diffFile Opts{..} parentDir ModFile{..} = do
	putStr $ "Writing diff " ++ enquote patchDest ++ "... "
	(_, sout, _, p) <- createProcess diffCmd
	diffData <- case sout of
		Just h -> BC.hGetContents h
		Nothing -> return BC.empty
	BC.writeFile patchDest diffData
	_ <- waitForProcess p
	putStrLn "done"
	where
	pickMod = if game == RTW then qsRTW else qsM2TW
	sourcePath = parentDir ++ "/" ++ name
	dest = (getModDataPath out (modName pickMod)) ++ name
	fname = snd $ splitFileName name
	patchDest = getGenPath out (modName pickMod) ++ fname ++ ".patch"
	diffCmd = CreateProcess
		{ cmdspec = ShellCommand $ concat
			[ "diff -auN "
			, dquote sourcePath
			, " "
			, dquote dest
			]
		, cwd = Nothing
		, env = Nothing
		, std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		, close_fds = False
		, create_group = False
		, delegate_ctlc = False
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
	{-
	 - Some text files are encoded in UTF-16LE, and begin with the initial
	 - sequence 0xfffe (which is the little-endian version of the optional
	 - 0xfeff byte order mark (BOM) in Unicode that signals the endianness of a
	 - text file or stream and is to be treated as the 0xfeff (U+FEFF) ZERO WITH
	 - NO-BREAK SPACE character after it is read). This is because RTW's engine
	 - is a giant pile of spaghetti code that uses different parsers for text
	 - files in an ad hoc manner; namely, those .txt files generated from
	 - spreadsheets (ugh!) use a different parser (the semicolon ';' is a
	 - comment character there) than those .txt files that use the not-sign '¬'
	 - (codepoint U+00AC) as the comment character.
	 -
	 - Anyway, since our regexes use the ByteString type and Char8, they work
	 - with 1 byte at a time, and will fail when they are run against UTF16. We
	 - could either use another non-standard module (Data.Text) to decode it
	 - into UTF8, or, just do it manually. Here, we do it manually with the
	 - utf16leToAscii function. After dumping the BOM (0xfffe), we extract only
	 - those bytes that are nonzero (we run the risk of encountering a UTF16
	 - character that actually uses more than 8 bits, but that has not yet
	 - happened thus far), then pass it on to our regexes.
	 -
	 - The game will crash if we don't preserve this UTF-16LE encoding, so we
	 - crudely encode the ASCII Char8 ByteString back to UTF-16LE (with the BOM)
	 - keep the game engine happy.
	 -}
	let
		l1 = B.index src 0
		l2 = B.index src 1
	if (nonconforming l1 l2)
		then BC.writeFile dest
			. addHeader
			. addZeroes
			. BC.append (BC.pack . (++"\r\n") $ cmtBox UTF16LE modinfo)
			. transform mf $ utf16leToAscii src
		else BC.writeFile dest
			. BC.append (BC.pack . (++"\r\n") $ cmtBox lang modinfo)
			$ transform mf src
	putStrLn "done"
	where
	pickMod = if game == RTW then qsRTW else qsM2TW
	mpd = getModDataPath out (modName pickMod)
	dest = mpd ++ name
	sourcePath = parentDir ++ "/" ++ name
	modinfo = modName pickMod ++ " version " ++ modVer pickMod
	lang = case (reverse . take 3 . reverse $ name) of
		"xml" -> XML
		_ -> Script
	nonconforming l1 l2 = (l1 == (0xff::Word8) && l2 == (0xfe::Word8))
	utf16leToAscii = B.filter (\b -> b /= (0x00::Word8)) . B.drop 2
	addZeroes = B.concatMap (\b -> (B.pack [b, (0x00::Word8)]))
	addHeader = BC.append (B.pack [0xff, 0xfe])

transform :: ModFile -> BC.ByteString -> BC.ByteString
transform ModFile{..} src
	| null . fst . partition $ operation = compose
		(map grpGsub (regexes operation)) $ src
	-- First split up the source string into subparts, then apply the regex
	-- transformations for each part.
	| otherwise = BC.intercalate
		(BC.pack (snd . partition $ operation))
		(map applyRegexes parts)
	where
	parts = splitBy (makeRegexDef $ BC.pack (fst . partition $ operation)) src
	applyRegexes = compose (map grpGsub (regexes operation))
