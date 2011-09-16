module Regex.RTW where

import Regex
import Data

_RTW_EDB_FUNCS :: TextRegex
_RTW_EDB_FUNCS = addTrueTest [r1]
    where
        -- All building constructions take 1 turn
        r1 =    [ ("^\\s+construction\\s+", id)
                , (_REGEX_INT, only "1")
                ]
