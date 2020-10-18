import System.Environment ( getArgs ) 
import Control.Monad ( replicateM )

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args >0
                      then read (head args)
                      else 0 :: Int
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)