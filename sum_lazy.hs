toInts :: String -> [Int]
toInts = map read . lines

sumOfSquares :: [Int] -> Int
sumOfSquares ns = sum $ map (^2) ns

main :: IO ()
main = do
  userInput <- getContents
  let nums = toInts userInput
  print (sumOfSquares nums)
