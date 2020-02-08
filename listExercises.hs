-- listExercises.hs

-- Q 6.1 repeat
myRepeat :: a -> [a]
myRepeat x = cycle [x]

-- Q 6.2 subseqience
mySubSeq :: Int -> Int -> [a] -> [a]
mySubSeq start end lis =
    drop start (take end lis)


-- Q 6.3 Function to determine if an element is the frst half of a list
-- Use integral division, which will round down for lisr with an odd number of elements
inFirstHalf :: (Eq a) => a -> [a] -> Bool
inFirstHalf e lis = 
    elem e (take ((length lis) `div` 2) lis)
