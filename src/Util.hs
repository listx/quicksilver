module Util where

import qualified Text.Printf as TP

import Option

dquote :: String -> String
dquote s = "\"" ++ s ++ "\""

enquote :: String -> String
enquote s = "`" ++ s ++ "'"

cmtRuler :: String -> String
cmtRuler s = replicate (length s) ';' ++ "\r\n"

cmtBox :: String -> String
cmtBox s = horiz ++ ";; " ++ s ++ " ;;\r\n" ++ horiz
    where
        horiz = replicate (length s + 6) ';' ++ "\r\n"

costsDiffNote :: String -> [Int] -> [Int] -> [Double] -> String
costsDiffNote s old new chg =
    cmtBox _QS_INFO
    ++ "\r\n; " ++ s ++ " changes from vanilla M2TW:\r\n"
    ++ showIntChanges (zip3 old new chg)
    ++ "\r\n\r\n"

showIntChanges :: [(Int, Int, Double)] -> String
showIntChanges = concatMap showChg
    where
        showChg (o, n, diff) = ";    " ++ show o ++ " -> " ++ show n ++ TP.printf " (%.2fx)\r\n" diff


-- Function composition over a list; see http://www.haskell.org/haskellwiki/Compose
compose :: [a -> a] -> a -> a
compose funcs = foldl (.) id (reverse funcs)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

thd3 :: (a, b, c) -> c
thd3 (a, b, c) = c
