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

-- using explicit handle. Move hClose after the hGetContents is used. In the book, it was after
-- the "let summary = ...", but that still failed. Maybe because summary wan't used, so the read
-- still wasn't forced? It works after the appendFile. But still doesn't work with stats.dat, since
-- closing file after trying to write to it.
main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadMode
    input <- hGetContents file
    let summary = (countsText . getCounts) input
    appendFile "stats.dat" (mconcat [filename, " ", summary, "\n"])
    hClose file
    putStrLn summary