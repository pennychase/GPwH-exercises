import System.Environment ( getArgs )
import System.IO ()

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
    where
        charCount = length input
        wordCount = (length . words) input
        lineCount = (length . words) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =
    mconcat ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

-- using ReadFile - handle is busy when trying to write to stats.dat
main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    input <- readFile filename
    let summary = (countsText . getCounts) input
    appendFile "stats.dat" (mconcat [filename, " ", summary, "\n"])
    putStrLn summary