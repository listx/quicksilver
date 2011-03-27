{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Control.Monad (when)
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as BL
import IO
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Verbosity
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Regex.PCRE hiding (match)
import Data.Array
import Text.Show.Pretty

_QS_NAME = "quicksilver"
_QS_VERSION = "0.01"
_QS_INFO = _QS_NAME ++ " version " ++ _QS_VERSION
_COPYRIGHT = "(C) Linus Arver 2011"

_DC     = "descr_character.txt"
_DCL    = "descr_cultures.txt"
_DFS    = "descr_faction_standing.txt"
_DP     = "descr_projectile.txt"
_DS     = "world/maps/campaign/imperial_campaign/descr_strat.txt"
_DW     = "descr_walls.txt"
_EDB    = "export_descr_buildings.txt"
_EDCT   = "export_descr_character_traits.txt"
_EDU    = "export_descr_unit.txt"
_SHA1_DC    = 0x9505d63491e7c90debba66016f4a837173fa8374
_SHA1_DCL   = 0x0d121a3b5567f1d326abde8f7f270c8fe6469952
_SHA1_DFS   = 0x6e9bc8a4ec4e938ba5124e049079758efe8e2ed2
_SHA1_DP    = 0xd8c67ee32e5856d16e4a0947f54c7b8b0fe9e653
_SHA1_DS    = 0x852dd3c9a4a6647f38af2f84eb07874de152acf8
_SHA1_DW    = 0x959d554513c313b12be95ac9579bff3b4aa49521
_SHA1_EDB   = 0xca2018a694fbabcaf5a058f4e87889acc3d35f89
_SHA1_EDCT  = 0x2a95300126b5f78294caffa0af8658f4576a2d40
_SHA1_EDU   = 0x140f93465e48577a5262d6e104630518426a7a13

data Opts = Opts
    { unpacked_data_path    :: FilePath
    , data_path             :: FilePath
    , no_sha                :: Bool
    } deriving (Data, Typeable, Show, Eq)

qsOpts :: Opts
qsOpts = Opts
    { unpacked_data_path    = def &= typDir &= help "path to the `data' directory created by the official M2TW unpacker.exe tool"
    , data_path             = def &= typDir &= help "path to the `data' directory inside your M2TW installation folder"
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

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ putStrLn "Use -h or --help." >> exitWith ExitSuccess
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    opts <- getOpts
    checkOpts opts
    qs opts

-- Basic option sanity check, as well as ensuring that source files are the official originals.
checkOpts :: Opts -> IO ()
checkOpts Opts{..} = do
    when (null unpacked_data_path) $ abort ("--unpacked-data-path is empty", 1)
    when (null data_path) $ abort ("--data-path is empty", 1)
    -- check that source files exist
    checkExists _DC     _DC_PATH
    checkExists _DCL    _DCL_PATH
    checkExists _DFS    _DFS_PATH
    checkExists _DP     _DP_PATH
    checkExists _DS     _DS_PATH
    checkExists _DW     _DW_PATH
    checkExists _EDB    _EDB_PATH
    checkExists _EDCT   _EDCT_PATH
    checkExists _EDU    _EDU_PATH
    -- check SHA1 sums
    when (not no_sha) $ do
        checkSum _DC    _DC_PATH    _SHA1_DC
        checkSum _DCL   _DCL_PATH   _SHA1_DCL
        checkSum _DFS   _DFS_PATH   _SHA1_DFS
        checkSum _DP    _DP_PATH    _SHA1_DP
        checkSum _DS    _DS_PATH    _SHA1_DS
        checkSum _DW    _DW_PATH    _SHA1_DW
        checkSum _EDB   _EDB_PATH   _SHA1_EDB
        checkSum _EDCT  _EDCT_PATH  _SHA1_EDCT
        checkSum _EDU   _EDU_PATH   _SHA1_EDU
    where
        _DC_PATH   = udp ++ "/" ++ _DC
        _DCL_PATH  = udp ++ "/" ++ _DCL
        _DFS_PATH  = udp ++ "/" ++ _DFS
        _DP_PATH   = udp ++ "/" ++ _DP
        _DS_PATH   = dp  ++ "/" ++ _DS
        _DW_PATH   = udp ++ "/" ++ _DW
        _EDB_PATH  = udp ++ "/" ++ _EDB
        _EDCT_PATH = udp ++ "/" ++ _EDCT
        _EDU_PATH  = udp ++ "/" ++ _EDU
        udp = unpacked_data_path
        dp = data_path

checkExists :: String -> FilePath -> IO ()
checkExists fname fpath = do
    putStr $ "checking if `" ++ fname ++ "' exists ... "
    fpathExist <- doesFileExist fpath
    when (not fpathExist) $ abort ("could not find `" ++ fname ++ "' source", 1)
    putStrLn "OK"

checkSum :: String -> FilePath -> Integer -> IO ()
checkSum fname fpath cksum = do
    putStr $ "checking SHA-1 sum of `" ++ fname ++ "' ... "
    fBytes <- BL.readFile fpath
    when (integerDigest (sha1 fBytes) /= cksum) $ abort (fname ++ ": SHA-1 mismatch", 2)
    putStrLn "OK"

qs :: Opts -> IO ()
qs opts@Opts{..} = do
    putStrLn "\nStarting mod generation ... "
    createGenDir
    createBat
    createCfg
    putStrLn $ "unpacked data path is: `" ++ unpacked_data_path ++ "'"
    putStrLn $ "data path is: `" ++ data_path ++ "'"
    writeDC _DC _DC_PATH
    putStrLn $ "\nquicksilver mod version " ++ _QS_VERSION ++ " successfully generated."
    where
        _DC_PATH   = udp ++ "/" ++ _DC
        _DCL_PATH  = udp ++ "/" ++ _DCL
        _DFS_PATH  = udp ++ "/" ++ _DFS
        _DP_PATH   = udp ++ "/" ++ _DP
        _DS_PATH   = dp  ++ "/" ++ _DS
        _DW_PATH   = udp ++ "/" ++ _DW
        _EDB_PATH  = udp ++ "/" ++ _EDB
        _EDCT_PATH = udp ++ "/" ++ _EDCT
        _EDU_PATH  = udp ++ "/" ++ _EDU
        udp = unpacked_data_path
        dp = data_path

createGenDir :: IO ()
createGenDir = do
    putStr "Ensuring that directory `./gen' exists ... "
    doesDirectoryExist "gen" >>= (\exist -> when (not exist) $ createDirectory "gen")
    putStrLn "OK"

createBat :: IO ()
createBat = do
    putStr "Writing quicksilver.bat ... "
    writeFile "gen/quicksilver.bat" contents
    putStrLn "done"
    where
        contents = "medieval2.exe @quicksilver.cfg"

createCfg :: IO ()
createCfg = do
    putStr "Writing quicksilver.cfg ... "
    writeFile "gen/quicksilver.cfg" contents
    putStrLn "done"
    where
        contents =  "[features]\n\
                    \mod = quicksilver\n\
                    \\n\
                    \[log]\n\
                    \to = logs/system.log.txt\n\
                    \level = * error\n\
                    \\n\
                    \[game]\n\
                    \event_cutscenes = 0\n\
                    \\n\
                    \[misc]\n\
                    \unlock_campaign = true"

writeDC :: String -> FilePath -> IO ()
writeDC fname sourceFpath = do
    putStr $ "Writing new `" ++ fname ++ "' ... "
    src <- readFile sourceFpath
    writeFile ("gen/" ++ _DC) (process src)
    putStrLn "done"
    where
        process = rep2 . rep1
        same str = str
        -- Base: Ship movement speed +50%
        rep1 =  grpGsub    [ ("type\\s+admiral.+?starting_action_points\\s+", same)
                           , ("\\d+", mult 1.5)
                           ]
        -- Global: Campaign movement speed +75%
        rep2 =  grpGsub    [ ("starting_action_points\\s+", same)
                           , ("\\d+", mult 1.75)
                           ]

-- takes a list of strings and functions, such as [("aaa", foo), ("bbb", bar)] and performs a global
-- substitution on them, as follows:
--      1. create a regex "(aaa)(bbb)"
--      2. replace "aaa"'s match by applying foo on it
--      3. replace "bbb"'s match by applying bar on it
--      4. etc. etc.
grpGsub :: [(String, String -> String)] -> String -> String
grpGsub grps src =
    sub cnt re funcs src
    where
        parenthesize s = "(" ++ s ++ ")"
        re = concatMap (parenthesize . fst) grps
        funcs = zip [1..] $ map snd grps
        cnt = length (match re src) - 1

-- process the results of a matchAllText with an association list of functions
sub :: Int -> String -> [(Int, String -> String)] -> String -> String
sub (-1) _ _ src = src
sub cnt re funcs src =
    sub (cnt - 1) re funcs (sub' minfo funcs src)
    where
        minfos = match re src
        minfo = minfos!!cnt

-- manually replaces text using MatchText info, but intelligently with an association list of string
-- manipulation functions (where key corresponds to the group that this function will act on)
sub' :: MatchText String -> [(Int, String -> String)] -> String -> String
sub' matchInfo assocFuncs src =
    take pos src ++ replacement ++ drop (pos + bytes) src
    where
        kvs = assocs matchInfo
        (fullMatchStr, (pos, bytes)) = matchInfo!0
        replacement = concatMap replaceGrps kvs
        replaceGrps (k, v@(str, (_, _))) = case lookup k assocFuncs of
            Just func -> func str
            _ -> ""

match :: String -> String -> [MatchText String]
match re = matchAllText (makeRegexDef re)

mult :: Double -> String -> String
mult d i = show . round $ fromIntegral (read i::Int) * d

abort :: (String, Int) -> IO ()
abort (msg, eid) = do
    errMsg msg
    hPutStrLn stderr "operation aborted"
    exitWith $ ExitFailure eid

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

-- only support backreferences \0 through \9, so that we can do "\19034" to mean "backreference \1,
-- followed by '9034'"
subRegex :: Regex                          -- ^ Search pattern
         -> String                         -- ^ Input string
         -> String                         -- ^ Replacement text
         -> String                         -- ^ Output string
subRegex _ "" _ = ""
subRegex regexp inp repl =
    let compile _i str [] = \ _m ->  (str++)
        compile i str (("\\",(off,len)):rest) =
            let i' = off+len
                pre = take (off-i) str
                str' = drop (i'-i) str
            in  if null str'
                    then \ _m -> (pre ++) . ('\\':)
                    else \  m -> (pre ++) . ('\\' :) . compile i' str' rest m
        compile i str ((xstr,(off,len)):rest) =
            let i' = off+len
                pre = take (off-i) str
                str' = drop (i'-i) str
                x = read xstr
            in  if null str'
                    then \ m -> (pre++) . ((fst (m!x))++)
                    else \ m -> (pre++) . ((fst (m!x))++) . compile i' str' rest m

        compiled :: MatchText String -> String -> String
        compiled = compile 0 repl findrefs
            where
                bre = makeRegexOpts defaultCompOpt execBlank "\\\\(\\\\|[0-9])"
                findrefs = map (\m -> (fst (m!1),snd (m!0))) (matchAllText bre repl)

        go _i str [] = str
        go i str (m:ms) =
            let (_,(off,len)) = m!0
                i' = off+len
                pre = take (off-i) str
                str' = drop (i'-i) str
            in  if null str'
                    then pre ++ (compiled m "")
                    else pre ++ (compiled m (go i' str' ms))
    in go 0 inp (matchAllText regexp inp)

-- Substitue re with repl in src using options copts and eopts.
reSub :: String -> String -> String -> String
reSub inp re repl = subRegex (makeRegexOpts copts eopts re) inp repl
    where
        copts = sum [compDotAll, compMultiline]
        eopts = sum [execBlank]

gsub :: String -> (String -> String) -> String -> String
gsub re f inp = case inp =~ re :: (Int,Int) of
    (-1,_)    -> inp
    (begin,n) -> take begin inp ++ f (take n (drop begin inp)) ++ gsub re f (drop n (drop begin inp))

makeRegexDef :: String -> Regex
makeRegexDef re = makeRegexOpts copts eopts re
    where
        copts = sum [compDotAll, compMultiline]
        eopts = sum [execBlank]
