import qualified Data.Map as M

-- Chapter 28 - Q28.3
-- Use the RobotParts DB and create a CLI with which a user is asked
-- to enter two part ids and return the cheaper

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

leftLeg :: RobotPart
leftLeg = RobotPart
    { name = "left leg"
    , description = "left leg for stomping kick"
    , cost = 1500.00
    , count = 7
    }

rightLeg :: RobotPart
rightLeg = RobotPart
    { name = "right leg"
    , description = "right leg for roundhouse kick"
    , cost = 1500.00
    , count = 7
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
        vals = [leftArm, rightArm, leftLeg, rightLeg, robotHead]
        keys = [1 .. length vals]

getCost :: Int -> M.Map Int RobotPart -> Maybe Double
getCost partId db = cost <$> part
    where
        part = M.lookup partId db


cmpParts :: Int -> Int -> String
cmpParts id1 id2 =
    case (<) <$> cost1 <*> cost2 of
        Just True   -> (show id1) ++ " is cheaper"
        Just False  -> (show id2) ++ " is cheaper"
        Nothing     -> "Part not found"
    where
        cost1 = getCost id1 partsDB
        cost2 = getCost id2 partsDB

cmpIds :: IO String
cmpIds = cmpParts <$> readInt <*> readInt

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
    putStrLn "Enter two robot part numbers:"
    comparison <- cmpIds
    print comparison

