module PTable where

import Data.Semigroup
import Data.List
import Data.List.Split
-- if we're doing this using glocal stack (and no stack.yaml with split as a dependency)
-- we need to have done: stack install split

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where
        nToAdd = length l2
        repeatedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2

data Events = Events [String]

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
    where
        combiner = (\x y -> mconcat [x, "-", y])

instance Semigroup Events where
    (<>) = combineEvents

instance Monoid Events where
    mempty = Events []

data Probs = Probs [Double]

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
    (<>) = combineProbs

instance Monoid Probs where
    mempty = Probs []

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events)
                                                    (Probs normalizedProbs)
    where
        totalProbs = sum probs
        normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, " | ", show prob, "\n"]

instance Show PTable where
    show (PTable (Events e) (Probs p)) = mconcat pairs
        where
            pairs = zipWith showPair e p

instance Semigroup PTable where
    (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
    (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable (e1 <> e2) (p1 <> p2)


data Coin = Head | Tail
    deriving (Show, Read, Enum)

data Die = One | Two | Three | Four | Five | Six
    deriving (Show, Read, Eq)

instance Enum Die where

    fromEnum s
        | s == One = 1
        | s == Two = 2
        | s == Three = 3
        | s == Four = 4
        | s == Five = 5
        | s == Six = 6

    toEnum s
        | s == 1 = One
        | s == 2 = Two
        | s == 3 = Three
        | s == 4 = Four
        | s == 5 = Five
        | s == 6 = Six


-- Create Events for multiple Dice

-- read the die side names from strings into the Die enumeration
readDieList :: [String] -> [Die]
readDieList = map read

-- sum the die sides when tossing multiple dice (having combined the tosses into a Ptable)
sumDieSides :: PTable -> [(Int, Double)]
sumDieSides (PTable (Events e) (Probs p)) = zip (map (sum . map fromEnum) strsToDice) p
    where strsToDice = map (\x -> readDieList (splitOn "-" x)) e

-- Given a PTable of the probs of tossing multiple dice, create a new PTable of the
-- sums of the tosses (i.e., when the two dice sum to 2, 3, 4, etc)
combineDice :: PTable -> PTable
combineDice dice = PTable (Events (map show sumSides)) (Probs probs)
    where
        groupedProbs = group (sortOn fst (sumDieSides dice))
        sumSides = map (fst . head) $  groupedProbs
        probs = map sum $ map (map snd) $  groupedProbs

coin = createPTable (Events (map show [Head, Tail])) (Probs [0.5, 0.5])
die = PTable (Events (map show [One, Two, Three, Four, Five, Six]))
             (Probs (take 6 . repeat $ 1.0/6))
twoDice = die <> die

-- now we can do "combineDice twoDice" to see the probabilities of the sums of
-- dice faces
