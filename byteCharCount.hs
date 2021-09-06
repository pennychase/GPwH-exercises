{-# LANGUAGE OverloadedStrings #-}

import System.Environment ( getArgs )
import System.IO
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getCounts :: Text -> (Int, Int)
getCounts input = (charCount, byteCount)
    where
        charCount = T.length input
        byteCount = (B.length . E.encodeUtf8) input

countsText :: (Int, Int) -> T.Text
countsText (cc, bc) =
    T.pack $ mconcat ["chars: ", show cc, " bytes: ", show bc]

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    input <-TIO.readFile filename
    let summary = (countsText . getCounts) input
    TIO.appendFile "stats.dat" (mconcat [T.pack filename, " ", summary, "\n"])
    TIO.putStrLn summary