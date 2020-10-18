import Data.Char ( isDigit )
import qualified Data.Text as T

data Expr = Add Int Int | Mul Int Int

parseExpr :: T.Text -> Expr
parseExpr str =
  case op of
    '+' -> Add n1 n2
    '*' -> Add n1 n2
  where
    n1 = read (T.unpack $ T.takeWhile isDigit str) :: Int
    rest = T.strip $ T.dropWhile isDigit str
    op = T.head rest
    n2 = read (T.unpack $ T.tail rest) :: Int

calc :: [T.Text] -> [Int]
calc strs =
  map calc' strs
  where
    calc' str =
      case parseExpr str of
        Add n1 n2 -> n1 + n2
        Mul n1 n2 -> n1 * n2

main :: IO ()
main = do
  userInput <- getContents
  let userInput' = T.pack userInput
  let exprs = T.lines userInput'
  print (calc exprs)
