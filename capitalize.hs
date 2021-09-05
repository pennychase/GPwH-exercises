import System.Environment ( getArgs )
import System.IO ( IOMode(ReadMode, WriteMode), openFile, hClose )
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

-- Exercise for Chapter 24
-- Capitalize and overwrite file

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    fh <- openFile filename ReadMode
    contents <- TIO.hGetContents fh
    hClose fh
    let capitalized = T.toUpper contents
    fh <- openFile filename WriteMode
    TIO.hPutStr fh capitalized
    hClose fh

