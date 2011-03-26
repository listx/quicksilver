import Control.Monad (when)
import IO
import System.Directory
import System.IO
import Text.Regex

_QS_VERSION = "0.01"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    putStrLn "\nBeginning mod generation..."
    createGenDir
    createBat
    createCfg
    putStrLn $ "\nquicksilver mod version " ++ _QS_VERSION ++ " successfully generated."

createGenDir :: IO ()
createGenDir = do
    putStr "Ensuring that directory ./gen exists..."
    doesDirectoryExist "gen" >>= (\exist -> when (not exist) $ createDirectory "gen")
    putStrLn "done"

createBat :: IO ()
createBat = do
    putStr "Writing quicksilver.bat..."
    writeFile "gen/quicksilver.bat" contents
    putStrLn "done"
    where
        contents = "medieval2.exe @quicksilver.cfg"

createCfg :: IO ()
createCfg = do
    putStr "Writing quicksilver.cfg..."
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
