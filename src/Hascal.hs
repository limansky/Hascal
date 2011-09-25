-- main module

import Event
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO

data Flag = Import String deriving Show

options = 
    [ Option ['i']      ["import"]      ( ReqArg Import "FILE" )   "file to be imported"
    ]

main = do
    putStrLn "Welcome to Hascal world :)"
    args <- getArgs
    case getOpt RequireOrder options args of 
        (flags, [], []) -> mapM doOpt flags
        (_, _, msgs)    -> error $ concat msgs ++ usage
        where usage = usageInfo usageHeader options


usageHeader = "Usage: hascal [OPTIONS]:"

doOpt (Import fn) = importICS fn

importICS fn = do
    putStrLn $ "Importing file:" ++ fn
    h <- openFile fn ReadMode
    cont <- hGetContents h
    putStrLn . show $ icalToEvent cont
