module Util where

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

-- Function composition over a list; see http://www.haskell.org/haskellwiki/Compose
compose :: [a -> a] -> a -> a
compose funcs = foldl (.) id (reverse funcs)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

thd3 :: (a, b, c) -> c
thd3 (a, b, c) = c
