{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Data where

import Data.Data
import qualified Data.ByteString.Char8 as BC

import Util

{-
 - Game type.
 -
 - RTW: Rome: Total War (1.5)
 - M2TW: Medieval II Total War (1.3)
 -}
data Game
    = RTW
    | M2TW
    deriving (Data, Typeable, Eq, Show)

{-
 - Where does the vanilla data come from?
 -
 - Installed: file comes from the installation directory Unpacked: file can only
 - be accessed from the unpacked data directory (using the official unpacker
 - utility)
 -}
data DataOrigin
    = Installed
    | Unpacked
    deriving (Eq, Show)

{-
 - Method of creating the modified file.
 -
 - Text: do text manipulations (regex replace).
 - Binary: perform a binary diff on the vanilla file.
 -}
data Operation
    = ModText   { regexes :: TextRegex
                , partition :: (String, String)
                }
    | ModBinary { bdiff :: FilePath
                }
    | Copy

type TextRegex = [[(String, BC.ByteString -> BC.ByteString, BC.ByteString -> Bool)]]

data ModFile = ModFile
    { sha :: Integer
    , name :: FilePath
    , origin :: DataOrigin
    , operation :: Operation
    }

instance Show ModFile where
    show ModFile{..} =  s' ++ n' ++ o' ++ op'
        where
            s' = "sha: " ++ showHex sha ++ " "
            n' = "name: " ++ show name ++ " "
            o' = "origin: " ++ o'' ++ " "
            o'' = case origin of
                Installed -> show "Installed "
                _ -> show "Unpacked "
            op' = "operation: " ++ op''
            op'' = case operation of
                ModText _ _ -> "text"
                ModBinary _ -> "binary"
                _ -> "copy"

type Mod = (Game, [ModFile])

makeModFileModText :: (Integer, String, Operation) -> DataOrigin -> ModFile
makeModFileModText (a, b, c) o = ModFile a b o c

makeModFile :: (Integer, String) -> DataOrigin -> Operation -> ModFile
makeModFile (a, b) ori op = ModFile a b ori op
