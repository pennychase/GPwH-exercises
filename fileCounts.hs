import System.Environment ( getArgs )
import System.IO ( openFile, hGetContents, hClose, IOMode (..) )

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
    where
        charCount = length input
        wordCount = (length . words) input
        lineCount = (length . words) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =
    mconcat ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

-- using explicit handle. hClose closes the handle before input is used, so hGetContents 
-- actually tries the read in the following expression
main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadMode
    input <- hGetContents file
    hClose file
    let summary = (countsText . getCounts) input
    appendFile "stats.dat" (mconcat [filename, " ", summary, "\n"])
    putStrLn summary