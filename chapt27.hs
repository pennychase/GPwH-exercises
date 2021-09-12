import qualified Data.Map as M
import Data.Char

-- Q27.1

data Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box b) = Box (f b)

morePresents :: Int -> Box a -> Box [a]
morePresents n b = (take n . repeat) <$> b

-- Q27.2
myBox :: Box Int
myBox = Box 1

-- function to put a Box's value in another Box
newBox :: Box a -> Box a
newBox = fmap id

-- unwrapping
wrapped = fmap Box myBox

unwrap :: Box a -> a
unwrap (Box b) = b

-- Q27.3

data RobotPart = RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
    { name = "left arm"
    , description = "left arm for face punching"
    , cost = 1000.00
    , count = 3
    }

rightArm :: RobotPart
rightArm = RobotPart
    { name = "right arm"
    , description = "right arm for kind hand gesture"
    , cost = 1025.00
    , count = 5
    }

robotHead :: RobotPart
robotHead = RobotPart
    { name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
    }

partsDB :: M.Map Int RobotPart
partsDB = M.fromList (zip keys vals)
    where
        vals = [leftArm, rightArm, robotHead]
        keys = [1 .. length vals]

lookupCost :: Int -> Maybe Double
lookupCost n = cost <$> M.lookup n partsDB

process :: String -> Maybe Double
process input = 
    if (input /= "" && all isDigit input)
    then lookupCost (read input :: Int)
    else Nothing

displayResult :: Maybe Double -> String
displayResult Nothing = "Item not found"
displayResult (Just n) = show n

main :: IO ()
main = do
    input <- getLine
    print $ (displayResult . process) input
