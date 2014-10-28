module Error where

import System.Exit
import System.IO

abort :: (String, Int) -> IO ()
abort (msg, eid) = do
	errMsg msg
	hPutStrLn stderr "operation aborted"
	exitWith $ ExitFailure eid

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg
