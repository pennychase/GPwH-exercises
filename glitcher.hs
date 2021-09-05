import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Environment
import System.IO
import System.Random

--
-- Inserting random bytes
--

-- Convert Int to Char safely (ASCII values are [0..255])
intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255

-- Convert Int to ByteString.Char8 by first converting Int to Char
intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

-- Replace a byte in a ByteString with a new value
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
    where
        (before, rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC charVal

-- Use random numbers to generate args for replaceByte
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1, bytesLength)
    charVal <- randomRIO (0, 255)
    return (replaceByte location charVal bytes)

--
-- Sorting random bytes
--

-- Split a ByteString and take a chunk of the second half, sort it, and
-- glue the pieces together.
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
    where
        (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest
        changed = BC.reverse (BC.sort target)

-- Use random numbers to generate args for sortSection
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [ randomReplaceByte
                , randomSortSection
                , randomReplaceByte
                , randomSortSection
                , randomReplaceByte
                , randomSortSection
                ]

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    -- glitched <- randomReplaceByte imageFile
    -- glitched <- randomSortSection imageFile
    glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    print "all done"