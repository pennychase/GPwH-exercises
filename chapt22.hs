import System.Environment
import Control.Monad

-- IO examples from Chapter 22
-- To run, in main uncomment the example you want to run
-- Use runghc: runghc chapt22.hs [args]

-- Sum N numbers, where N is an command-line argument
sumMain :: IO ()
sumMain = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int

  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)

-- Print 3 lines of input
exampleMain :: IO ()
exampleMain = do
  vals <- mapM (\_ -> getLine) [1 .. 3]
  mapM_ putStrLn vals

-- Print input as a stream
-- Input is buffered, so nothing is printed intil a \n is entered. End input with ^D
lazyIOMain :: IO ()
lazyIOMain = do
  userInput <- getContents
  mapM_ print userInput

-- Reverse input stream
-- Doesn't print until end of input (^D)
reverser :: IO ()
reverser = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed

-- Version 2 of summing user-supplied numbers, each on a line
-- This time we separate the pure and IO parts of the program
-- by treating the input as a stream

toInts :: String -> [Int]
toInts = map read . lines

sumMain2 :: IO ()
sumMain2 = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)


main :: IO ()
main = do
  -- sumMain
  -- exampleMain
  -- lazyIOMain
  -- reverser
  sumMain2
