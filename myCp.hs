
import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure), exitWith )
import System.IO ()
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Exercise for Chapter 24

-- Argument handling

-- Simple parser
parse :: [String] -> IO ()
parse [] = usage >> exit
parse [_] = usage >> exit
parse [src, dest] = if src == dest
                    then usage >> exit
                    else copy src dest

-- Print usage on error
usage :: IO ()
usage = putStrLn "usage: myCp source destination"

-- Exit
exit :: IO ()
exit = exitWith (ExitFailure 1)

-- copy file
copy :: String -> String -> IO ()
copy srcFileName destFileName = do
    contents <- TIO.readFile srcFileName
    TIO.writeFile destFileName contents

main :: IO ()
main = getArgs >>= parse
