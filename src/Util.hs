module Util where

import qualified Text.Printf as TP

data Lang
    = XML
    | Script
    deriving (Eq)

dquote :: String -> String
dquote s = "\"" ++ s ++ "\""

enquote :: String -> String
enquote s = "`" ++ s ++ "'"

concatRep :: Int -> String -> String
concatRep n str = concat $ replicate n str

cmtRuler :: String -> String
cmtRuler s = replicate (length s) ';' ++ "\r\n"

cmtBox :: Lang -> String -> String
cmtBox lang s = case lang of
    XML -> horizXML ++ "<!-- " ++ s ++ " -->\r\n" ++ horizXML
    _ -> horizS ++ ";; " ++ s ++ " ;;\r\n" ++ horizS
    where
        horizXML = "<!--" ++ replicate (length s + 2) ' ' ++ "-->" ++ "\r\n"
        horizS = replicate (length s + 6) ';' ++ "\r\n"

costsDiffNote :: String -> [Int] -> [Int] -> [Double] -> String
costsDiffNote s old new chg =
    "\r\n; " ++ s ++ " changes from vanilla:\r\n"
    ++ showIntChanges (zip3 old new chg)
    ++ "\r\n\r\n"

showIntChanges :: [(Int, Int, Double)] -> String
showIntChanges = concatMap showChg
    where
        showChg (o, n, diff) = ";    " ++ show o ++ " -> " ++ show n ++ TP.printf " (%.2fx)\r\n" diff

-- Show a (string) double value, with up to 4 digits after the decimal point.
showD :: Double -> String
showD = TP.printf "%.6f"

showHex :: Integer -> String
showHex a
    | a < 0 = "negative value!"
    | otherwise = buildStr a []
    where
        buildStr :: Integer -> String -> String
        buildStr n s
            | n - r == 0 = (toHexChar r):s
            | otherwise = buildStr (div (n - r) 16) ((toHexChar r):s)
            where
                r = rem n 16
        toHexChar :: Integer -> Char
        toHexChar n = case n of
                0 -> '0'
                1 -> '1'
                2 -> '2'
                3 -> '3'
                4 -> '4'
                5 -> '5'
                6 -> '6'
                7 -> '7'
                8 -> '8'
                9 -> '9'
                10 -> 'a'
                11 -> 'b'
                12 -> 'c'
                13 -> 'd'
                14 -> 'e'
                _ -> 'f'

-- Function composition over a list; see http://www.haskell.org/haskellwiki/Compose
compose :: [a -> a] -> a -> a
compose funcs = foldl (.) id (reverse funcs)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
