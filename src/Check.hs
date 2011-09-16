{-# LANGUAGE RecordWildCards #-}
module Check where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.SHA
import System.Directory

import Data
import Error
import Option
import Util

checkExists :: FilePath -> IO ()
checkExists sourcePath = do
    putStr $ "Checking if " ++ enquote sourcePath ++ " exists... "
    sourceExist <- doesFileExist sourcePath
    when (not sourceExist) $ abort ("could not find " ++ enquote sourcePath, 1)
    putStrLn "OK"

checkSum :: FilePath -> Integer -> IO ()
checkSum sourcePath cksum = do
    putStr $ "Checking SHA-1 sum of " ++ enquote sourcePath ++ "... "
    sourceBytes <- BL.readFile sourcePath
    when (integerDigest (sha1 sourceBytes) /= cksum) $ abort (enquote sourcePath ++ ": SHA-1 mismatch", 2)
    putStrLn "OK"

checkFile :: Opts -> FilePath -> FilePath -> ModFile -> IO ()
checkFile Opts{..} idd udd ModFile{..}
    | no_check = return ()
    | no_sha = checkExists sourcePath
    | otherwise = checkSum sourcePath sha
    where
        sourcePath = parentDir ++ "/" ++ name
        parentDir = if origin == Installed
            then idd
            else udd
