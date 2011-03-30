module Check where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.SHA
import System.Directory
import System.FilePath

import Error

checkExists :: FilePath -> IO ()
checkExists sourcePath = do
    putStr $ "Checking if `" ++ sourcePath ++ "' exists... "
    sourceExist <- doesFileExist sourcePath
    when (not sourceExist) $ abort ("could not find `" ++ sourcePath ++ "'", 1)
    putStrLn "OK"

checkSum :: FilePath -> Integer -> IO ()
checkSum sourcePath cksum = do
    putStr $ "Checking SHA-1 sum of `" ++ sourcePath ++ "'... "
    sourceBytes <- BL.readFile sourcePath
    when (integerDigest (sha1 sourceBytes) /= cksum) $ abort (sourcePath ++ ": SHA-1 mismatch", 2)
    putStrLn "OK"
