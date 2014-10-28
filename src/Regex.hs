module Regex where

import Data.Array
import Data.List (foldl')
import qualified Data.ByteString.Char8 as BC
import Text.Regex.PCRE hiding (match)
import qualified Text.Printf as TP

import Util

_REGEX_INT, _REGEX_DOUBLE, _REGEX_LINE :: String
_REGEX_INT = "\\d+"
_REGEX_DOUBLE = "\\d+\\.\\d+"
_REGEX_LINE = "[^\\r]+\\r\\n"

type RegexSets = [[(String, BS -> BS, BS -> Bool)]]

nil :: BS -> BS
nil _ = BC.empty

only :: String -> BS -> BS
only repl _ = BC.pack repl

prepend :: String -> BS -> BS
prepend str orig = BC.append (BC.pack str) orig

add :: String -> BS -> BS
add str orig = BC.append orig $ BC.pack str

strElemTest :: [BS] -> BS -> Bool
strElemTest strs byteStr = elem byteStr strs

numTest :: (Int -> Bool) -> BS -> Bool
numTest f byteStr = case BC.readInt byteStr of
	Just (n, _) -> (f n)
	_ -> False

addTrueTest :: [[(String, (BS -> BS))]] -> RegexSets
addTrueTest = map (map (\(a, b) -> (a, b, alwaysTrue)))

alwaysTrue :: BS -> Bool
alwaysTrue _ = True

multRoundInt :: Double -> (String, BS -> BS)
multRoundInt m = (_REGEX_INT, mult m)

multDouble :: Double -> (String, BS -> BS)
multDouble m = (_REGEX_DOUBLE, mult' m)

-- Produce an integer value.
mult :: Double -> BS -> BS
mult d n = BC.pack $ show ((round $ n' * d)::Integer)
	where
	n' :: Double
	n' = if BC.isInfixOf (BC.pack ".") n
			then read (BC.unpack n)::Double
			else case BC.readInt n of
				Just (num, _) -> fromIntegral num
				_ -> 0.0

-- Produce a double value, with up to 4 digits after the decimal point.
mult' :: Double -> BS -> BS
mult' d n = BC.pack . TP.printf "%.4f" $ n' * d
	where
	n' = if BC.isInfixOf (BC.pack ".") n
			then read (BC.unpack n)::Double
			else case BC.readInt n of
				Just (num, _) -> fromIntegral num
				_ -> 0.0

applyFormula :: (Double -> Int) -> [Int] -> [Int]
applyFormula f ns = map (f . fromIntegral) ns

gradCost :: (Double -> Int) -> BS -> BS
gradCost formula i = BC.pack . show . formula $ fromIntegral i'
	where
	i' = case BC.readInt i of
			Just (n, _) -> n
			_ -> 0

getChgFactors :: [Int] -> [Int] -> [Double]
getChgFactors new old = map getChgFactor (zip new old)
	where
	getChgFactor :: (Int, Int) -> Double
	getChgFactor (n, o) = (fromIntegral n)/(fromIntegral o)

-- takes a list of strings and functions, such as [("aaa", foo), ("bbb", bar)] and performs a global
-- substitution on them, as follows:
--      1. create a regex "(aaa)(bbb)"
--      2. replace "aaa"'s match by applying foo on it
--      3. replace "bbb"'s match by applying bar on it
--      4. etc. etc.
grpGsub :: [(String, BS -> BS, BS -> Bool)] -> BS -> BS
grpGsub grps src =
	sub cnt re funcs src
	where
	parenthesize s = BC.pack $ "(" ++ s ++ ")"
	re = BC.concat $ map (parenthesize . fst3) grps
	funcs = zip3 [1..] (map snd3 grps) (map thd3 grps)
	cnt = length (match re src) - 1

-- Process the results of a matchAllText with an association list of regex replacement functions;
-- replacement occurs only if every subgroup regex's accompanying test function passes. E.g., if
-- funcs is
--     [ (1 , id, alwaysTrue)
--     , (2 , add ", free_upkeep_unit", \s -> not $ BC.isInfixOf (BC.pack "free_upkeep_unit") s)
--     , (3, id, alwaysTrue)
--     , (4, id, numTest (<= 5))
--     , (5, id, alwaysTrue)
--     , (6, id, numTest (<= 650))
--     ]
-- then funcs' is
--     [ (1 , id)
--     , (2 , add ", free_upkeep_unit")
--     , (3,  id)
--     , (4,  id)
--     , (5,  id)
--     , (6,  id)
--     ]
-- and funcs'' is
--     [ (1 , alwaysTrue)
--     , (2 , \s -> not $ BC.isInfixOf (BC.pack "free_upkeep_unit") s)
--     , (3, alwaysTrue)
--     , (4, numTest (<= 5))
--     , (5, alwaysTrue)
--     , (6, numTest (<= 650))
--     ]
-- . The regex looks (essentially) like "(\\1)(\\2)(\\3)(\\4)(\\5)(\\6)" because of grpGsub. The
-- entire regex's successful match (subgroups 1-6) are ONLY processed by the replacement functions
-- in funcs'(subgroups 1-6) if ALL of the tests in funcs'' return True.
sub :: Int -> BS -> [(Int, BS -> BS, BS -> Bool)] -> BS -> BS
sub (-1) _ _ src = src
sub cnt re funcs src =
	sub (cnt - 1) re funcs maybeSub
	where
	minfos = match re src
	minfo = minfos!!cnt
	funcs' = zip (map fst3 funcs) (map snd3 funcs)
	funcs'' = zip (map fst3 funcs) (map thd3 funcs)
	maybeSub = if testsOK
		then sub' minfo funcs' src
		else src
	testsOK = all (== True) tests
		where
		kvs = assocs minfo
		tests = map test kvs
		test (k, (str, (_, _))) = case lookup k funcs'' of
			Just func -> func str
			_ -> True -- since minfo includes the entire regex's full match info as the first (k, v), we will always visit this codepath (since we zip up the replacment functions starting from k = 1, and the full match info's k is 0), so we need to let the k = 0 case succeed.

-- manually replaces text using MatchText info, but intelligently with an association list of string
-- manipulation functions (where key corresponds to the group that this function will act on)
sub' :: MatchText BS -> [(Int, BS -> BS)] -> BS -> BS
sub' matchInfo assocFuncs src =
	BC.append (BC.append (BC.take (fromIntegral pos) src) replacement) (BC.drop (fromIntegral (pos + bytes)) src)
	where
	kvs = assocs matchInfo
	(_, (pos, bytes)) = matchInfo!0
	-- We use foldl' here to incrementally construct the full replacement bytestring; if we were
	-- to use BC.concat $ map ..., then the thunk size will grow (possibly very large) depending
	-- on the number of regex groups.
	replacement = foldl' BC.append BC.empty $ map replaceGrps kvs
	replaceGrps (k, (str, (_, _))) = case lookup k assocFuncs of
		Just func -> func str
		_ -> BC.empty -- we will always visit this codepath because k = 0 exists for the fullMatchStr case; since we only do replacments on a per-group (k = 1, k = 2, etc.) basis, we skip the fullMatchStr case by making it an empty string

match :: BS -> BS -> [MatchText BS]
match re = matchAllText (makeRegexDef re)

splitBy :: Regex -> BS -> [BS]
splitBy delim strIn
	| BC.null strIn = []
	| otherwise = let
		matches = map (!0) (matchAll delim strIn)
		go _ str [] = str : []
		go i str ((off,len):rest) = let
			i' = off+len
			firstline = BC.take (off-i) str
			remainder = BC.drop (i'-i) str
			in seq i' $
				if BC.null remainder
					then [firstline, BC.empty]
					else firstline : go i' remainder rest
		in go 0 strIn matches

makeRegexDef :: BS -> Regex
makeRegexDef re = makeRegexOpts copts eopts re
	where
	copts = sum [compDotAll, compMultiline]
	eopts = sum [execBlank]
