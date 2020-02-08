import Data.Char

-- Q1 - Write elem using length and filter
myElem :: (Eq a) => a -> [a] -> Bool
myElem e lis = length (filter (== e) lis) > 0


-- Q2 - Modify isPalidrome to handle strings with uppercase (by converting to lowercase) and spaces/punctuation
isPalindrome :: String -> Bool
isPalindrome xs = reverse xs' == xs'
    where
        xs' = map toLower (filter isAlpha xs)

ex1 = "A man, a plan, a canal, Panama!"
ex2 = "Able was I ere I saw Elba"

-- Q3 - sum harmonic series
harmonic n = foldl (+) 0 (map (1 /) [1 .. n])

harmonic' n =sum (take n seriesValues)
    where
        seriesPairs = zip (cycle [1.0]) [1.0, 2.0 ..]
        seriesValues = map (\pair -> (fst pair) / (snd pair)) seriesPairs