{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Control.Monad (when)
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import IO
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Verbosity
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Regex.PCRE hiding (match)
import Data.Array
import Data.List (foldl')
import Text.Show.Pretty

_QS_NAME = "quicksilver"
_QS_VERSION = "0.02"
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
    putStrLn $ "data path: `" ++ data_path opts ++ "'"
    putStrLn $ "unpacked data path: `" ++ unpacked_data_path opts ++ "'"
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
    putStr $ "checking if `" ++ fname ++ "' exists... "
    fpathExist <- doesFileExist fpath
    when (not fpathExist) $ abort ("could not find `" ++ fname ++ "' source", 1)
    putStrLn "OK"

checkSum :: String -> FilePath -> Integer -> IO ()
checkSum fname fpath cksum = do
    putStr $ "checking SHA-1 sum of `" ++ fname ++ "'... "
    fBytes <- BL.readFile fpath
    when (integerDigest (sha1 fBytes) /= cksum) $ abort (fname ++ ": SHA-1 mismatch", 2)
    putStrLn "OK"

qs :: Opts -> IO ()
qs opts@Opts{..} = do
    putStrLn "\nStarting mod generation... "
    createGenDir
    createBat
    createCfg
    write   _DC     _DC_PATH    _DC_FUNCS
    write   _DCL    _DCL_PATH   _DCL_FUNCS
    write   _DFS    _DFS_PATH   _DFS_FUNCS
    write   _DS     _DS_PATH    _DS_FUNCS
    write   _DW     _DW_PATH    _DW_FUNCS
    write   _EDCT   _EDCT_PATH  _EDCT_FUNCS
    write'  _EDB    _EDB_PATH   _EDB_FUNCS  "^\\}\\r\\n"        "}\r\n"
    write'  _EDU    _EDU_PATH   _EDU_FUNCS  " \\r\\n \\r\\n"    " \r\n \r\n"
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

_DC_FUNCS = [r1, r2, r3, rN]
    where
        -- Ship movement speed 1.5x
        r1 =    [ ("^type\\s+admiral.+?starting_action_points\\s+", id)
                , ("\\d+", mult 1.5)
                ]
        -- Diplomat movement speed 2x
        r2 =    [ ("^type\\s+diplomat.+?starting_action_points\\s+", id)
                , ("\\d+", mult 2)
                ]
        -- Princess movement speed 1.75x
        r3 =    [ ("^type\\s+princess.+?starting_action_points\\s+", id)
                , ("\\d+", mult 1.75)
                ]
        -- Campaign movement speed 1.75x
        rN =    [ ("^starting_action_points\\s+", id)
                , ("\\d+", mult 1.75)
                ]

_DCL_FUNCS = [rN]
    where
        -- Spy recruitment cost 3x
        rN =    [ ("^spy.+?spy\\.tga\\s+", id)
                , ("\\d+", mult 3)
                ]

_DFS_FUNCS = [rN]
    where
        -- Fixed faction standing bug
        rN =    [ ("^;Trigger 0102_city_razed.+?;-+", nil)
                ]

_DS_FUNCS = [r1, r2, rN]
    where
        -- Rebel spawn rate 10x lower
        r1 =    [ ("^brigand_spawn_value\\s+", id)
                , ("\\d+", mult 10)
                ]
        -- Pirate spawn rate 10x lower
        r2 =    [ ("^pirate_spawn_value\\s+", id)
                , ("\\d+", mult 10)
                ]
        -- King's purse 2x
        rN =    [ ("^denari_kings_purse\\s+", id)
                , ("\\d+", mult 2)
                ]

_DW_FUNCS = [r1, r2, r3, r4, r5, r6, r7, r8, rN]
    where
        -- Walls and gate HP 5x
        r1 =    [ ("^\\s+gate\\s.+?full_health\\s", id)
                , ("\\d+", mult 5)
                ]
        r2 =    [ ("^\\s+wall.+?full_health\\s", id)
                , ("\\d+", mult 5)
                ]
        r3 =    [ ("^\\s+gateway.+?full_health\\s", id)
                , ("\\d+", mult 5)
                ]
        -- Tower HP 2x
        r4 =    [ ("^\\s+tower.+?full_health\\s", id)
                , ("\\d+", mult 2)
                ]
        -- Tower non-flaming firing rate 2x
        r5 =    [ ("^\\s+fire_rate\\s+small\\s+", id)
                , ("\\d+", mult 0.5)
                ]
        r6 =    [ ("^\\s+fire_rate\\s+normal\\s+", id)
                , ("\\d+", mult 0.5)
                ]
        r7 =    [ ("^\\s+fire_rate\\s+large\\s+", id)
                , ("\\d+", mult 0.5)
                ]
        r8 =    [ ("^\\s+fire_rate\\s+huge\\s+", id)
                , ("\\d+", mult 0.5)
                ]
        -- City/Castle defense tower activation range 8x
        rN =    [ ("^\\s+control_area_radius\\s+", id)
                , ("\\d+", mult 8)
                ]

_EDCT_FUNCS = [rN]
    where
        -- Remove corruption trigger based on high treasury
        rN =    [ ("^Trigger corruption.+?;-+", nil)
                ]

_EDB_FUNCS = [r1, r2, r3, r4, r5, r6, r7, r8, r9, rN]
    where
        -- Mines give 5x profits
        r1 =    [ ("^\\s+mine_resource\\s+", id)
                , ("\\d+", mult 5)
                ]
        -- Mines cost 2x more
        r2 =    [ ("^\\s+mines.+?", id)
                , ("cost\\s+", id)
                , ("\\d+", mult 2)
                ]
        -- All building constructions take 1 turn
        r3 =    [ ("^\\s+construction\\s+", id)
                , ("\\d+", (\s -> BC.pack "1"))
                ]
        -- All building costs 1.33x
        r4 =    [ ("^\\s+cost\\s+", id)
                , ("\\d+", mult 1.33)
                ]
        -- Give free upkeep slots to castles (vanilla cities are 2, 3, 4, 5, 6)
        r5 =    [ ("^\\s{8}motte_and_bailey.+?wall_level.+?", id)
                , ("\\d+", up "1")
                ]
        r6 =    [ ("^\\s{8}wooden_castle.+?wall_level.+?", id)
                , ("\\d+", up "2")
                ]
        r7 =    [ ("^\\s{8}castle.+?wall_level.+?", id)
                , ("\\d+", up "3")
                ]
        r8 =    [ ("^\\s{8}fortress.+?wall_level.+?", id)
                , ("\\d+", up "4")
                ]
        r9 =    [ ("^\\s{8}citadel.+?wall_level.+?", id)
                , ("\\d+", up "5")
                ]
        -- All free upkeep slots 2.5x
        rN =    [ ("^\\s+free_upkeep\\s+bonus\\s+", id)
                , ("\\d+", mult 2.5)
                ]
        up :: String -> (BC.ByteString -> BC.ByteString)
        up amt = (\d -> BC.append d (BC.pack $ "\r\n" ++ replicate 16 ' ' ++ "free_upkeep bonus " ++ amt))

_EDU_FUNCS = [r1, r2]
    where
        -- Bodyguard soldiers (cavalry and infantry types) reduced 0.5x; costs reduced accordingly
        -- NOTE: Technically, this regex only catches "real" bodyguards from the campaign game;
        -- hero units in custom battles (King Richard, Duke William, etc.) are also heavy cavalry
        -- general's bodyguard units, but they are left alone.
        r1 =    [ ("^dictionary\\s+\\w+?_Bodyguard.+?soldier\\s+\\w+?_Bodyguard,\\s+", id)
                , ("\\d+", mult 0.5) -- soldiers 0.5x
                , (".+?stat_cost\\s+\\d+,\\s+", id)
                , ("\\d+", mult 0.5) -- recruitment cost
                , (",\\s+", id)
                , ("\\d+", mult 0.5) -- upkeep cost
                , (",\\s+", id)
                , ("\\d+", mult 0.5) -- weapon upgrade cost
                , (",\\s+", id)
                , ("\\d+", mult 0.5) -- armor upgrade cost
                , (",\\s+", id)
                , ("\\d+", mult 0.5) -- custom battle: recruitment cost
                , (",\\s+\\d+,\\s+", id) -- custom battle: recruitment count before penalty (skip)
                , ("\\d+", mult 0.5) -- custom battle: over-recruitment penalty
                ]
        -- Missile infantry ammo 1.75x
        r2 =    [ ("^category\\s+infantry\\s+class\\s+missile.+?stat_pri\\s+.+?,.+?,.+?,.+?,\\s+", id)
                , ("\\d+", mult 1.75)
                ]

nil :: BC.ByteString -> BC.ByteString
nil str = BC.empty

createGenDir :: IO ()
createGenDir = do
    putStr "Ensuring that directory `./gen' exists... "
    createDirectoryIfMissing True "gen"
    putStrLn "OK"
    putStr "Ensuring that directory `./gen/world/maps/campaign/imperial_campaign' exists... "
    createDirectoryIfMissing True "gen/world/maps/campaign/imperial_campaign"
    putStrLn "OK"

createBat :: IO ()
createBat = do
    putStr "Writing quicksilver.bat... "
    writeFile "gen/quicksilver.bat" contents
    putStrLn "done"
    where
        contents = "medieval2.exe @quicksilver.cfg"

createCfg :: IO ()
createCfg = do
    putStr "Writing quicksilver.cfg... "
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

write :: String -> FilePath -> [[(String, BC.ByteString -> BC.ByteString)]] -> IO ()
write fname sourceFpath funcs = do
    putStr $ "Writing new `" ++ fname ++ "'... "
    src <- BC.readFile sourceFpath
    BC.writeFile ("gen/" ++ fname) $ applyRegexes src
    putStrLn "done"
    where
        applyRegexes = compose (map grpGsub funcs)

-- First split up the source string into subparts, then apply the regex transformations for each
-- part.
write' :: String -> FilePath -> [[(String, BC.ByteString -> BC.ByteString)]] -> String -> String -> IO ()
write' fname sourceFpath funcs delim delim' = do
    putStr $ "Writing new `" ++ fname ++ "'... "
    src <- BC.readFile sourceFpath
    let parts = splitBy (makeRegexDef $ BC.pack delim) src
    BC.writeFile ("gen/" ++ fname) $ BC.intercalate (BC.pack delim') (map applyRegexes parts)
    putStrLn "done"
    where
        applyRegexes = compose (map grpGsub funcs)

-- Function composition over a list; see http://www.haskell.org/haskellwiki/Compose
compose :: [a -> a] -> a -> a
compose funcs = foldl (.) id (reverse funcs)

-- takes a list of strings and functions, such as [("aaa", foo), ("bbb", bar)] and performs a global
-- substitution on them, as follows:
--      1. create a regex "(aaa)(bbb)"
--      2. replace "aaa"'s match by applying foo on it
--      3. replace "bbb"'s match by applying bar on it
--      4. etc. etc.
grpGsub :: [(String, BC.ByteString -> BC.ByteString)] -> BC.ByteString -> BC.ByteString
grpGsub grps src =
    sub cnt re funcs src
    where
        parenthesize s = BC.pack $ "(" ++ s ++ ")"
        re = BC.concat $ map (parenthesize . fst) grps
        funcs = zip [1..] $ map snd grps
        cnt = length (match re src) - 1

-- process the results of a matchAllText with an association list of functions
sub :: Int -> BC.ByteString -> [(Int, BC.ByteString -> BC.ByteString)] -> BC.ByteString -> BC.ByteString
sub (-1) _ _ src = src
sub cnt re funcs src =
    sub (cnt - 1) re funcs (sub' minfo funcs src)
    where
        minfos = match re src
        minfo = minfos!!cnt

-- manually replaces text using MatchText info, but intelligently with an association list of string
-- manipulation functions (where key corresponds to the group that this function will act on)
sub' :: MatchText BC.ByteString -> [(Int, BC.ByteString -> BC.ByteString)] -> BC.ByteString -> BC.ByteString
sub' matchInfo assocFuncs src =
    BC.append (BC.append (BC.take (fromIntegral pos) src) replacement) (BC.drop (fromIntegral (pos + bytes)) src)
    where
        kvs = assocs matchInfo
        (fullMatchStr, (pos, bytes)) = matchInfo!0
        -- We use foldl' here to incrementally construct the full replacement bytestring; if we were
        -- to use BC.concat $ map ..., then the thunk size will grow (possibly very large) depending
        -- on the number of regex groups.
        replacement = foldl' BC.append BC.empty $ map replaceGrps kvs
        replaceGrps (k, v@(str, (_, _))) = case lookup k assocFuncs of
            Just func -> func str
            _ -> BC.empty

match :: BC.ByteString -> BC.ByteString -> [MatchText BC.ByteString]
match re = matchAllText (makeRegexDef re)

splitBy :: Regex -> BC.ByteString -> [BC.ByteString]
splitBy delim strIn
    | BC.null strIn = []
    | otherwise =
        let matches = map (!0) (matchAll delim strIn)
            go _ str [] = str : []
            go i str ((off,len):rest) =
                let i' = off+len
                    firstline = BC.take (off-i) str
                    remainder = BC.drop (i'-i) str
                in seq i' $
                    if BC.null remainder
                        then [firstline, BC.empty]
                        else firstline : go i' remainder rest
        in go 0 strIn matches

mult :: Double -> BC.ByteString -> BC.ByteString
mult d i = BC.pack . show . round $ fromIntegral (i') * d
    where
        i' = case BC.readInt i of
            Just (n, _) -> n
            _ -> 0

abort :: (String, Int) -> IO ()
abort (msg, eid) = do
    errMsg msg
    hPutStrLn stderr "operation aborted"
    exitWith $ ExitFailure eid

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

makeRegexDef :: BC.ByteString -> Regex
makeRegexDef re = makeRegexOpts copts eopts re
    where
        copts = sum [compDotAll, compMultiline]
        eopts = sum [execBlank]
