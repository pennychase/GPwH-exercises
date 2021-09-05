{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Semigroup

-- Exercise 23.1
helloPerson :: Text -> Text
helloPerson name = "Hello" <> " " <> name <> "!"

main1 :: IO ()
main1 = do
    TIO.putStrLn "Hello! What's your name?"
    name <- TIO.getLine
    let statement = helloPerson name
    TIO.putStrLn statement

-- Exercise 23.2
toInts :: Text -> [Int]
toInts ints = map (read . T.unpack) $ T.lines ints

printInt :: Int -> IO ()
printInt = TIO.putStrLn . T.pack . show

main2 :: IO ()
main2 = do
    userInput <- TIO.getContents
    let numbers = toInts userInput
    printInt (sum numbers)


main :: IO ()
main = do
    -- main1
    main2

