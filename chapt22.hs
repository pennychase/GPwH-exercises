exampleMain :: IO ()
exampleMain = do
  vals <- mapM (\_ -> getLine) [1 .. 3]
  mapM_ putStrLn vals

reverser :: IO ()
reverser = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed

main :: IO ()
main = do
  reverser
