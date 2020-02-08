import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

--
-- Time Series datatype
--

data TS a = TS [Int] [Maybe a]

-- create a Times Series from a list of times and a list of values
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
    where
        completeTimes = [ minimum times .. maximum times ]      -- create full time series, since there might be gaps
        timeValueMap = Map.fromList (zip times values)          -- create a Map of the data points we have
        extendedValues = map (\t -> Map.lookup t timeValueMap)  -- include missing data (lookup returns Nothing if time isn't in Map)
                             completeTimes

-- Helper function to read in files, create inputs for createTS, and then call createTS
fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS ts vs
    where
        (ts, vs) = unzip tvPairs

--
-- Make TS an instance of Show
--

-- showTVPair is the function that will be used by show
showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [ show time, " | ", show value, "\n" ]
showTVPair time Nothing = mconcat [ show time, " | NA\n" ]

-- Define instance
instance Show a => Show (TS a) where
    show (TS ts vs) = mconcat rows
        where rows = zipWith showTVPair ts vs

--
-- Merge Time Series
--

-- We're going to use a map of times and values to help merge two time series.
-- Since TS has Maybe values, define a helper function to remove values from Maybe context
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap      -- return the map if the value is Nothing
insertMaybePair myMap (k, Just v) = Map.insert k v myMap

-- combine two time series
-- Build a Map of times/values from the first time series and then insert the times/values
-- from the second series into the Map. We extend the times to cover all the time points in each
-- (including gaps) and if there are different values for the same time, we use the value from the 
-- second series (becuase we'll insert it into the Map, overwriting the orignal value)
combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where
        bothTimes = mconcat [t1, t2]    -- join the times from both time series
        completeTimes = [minimum bothTimes .. maximum bothTimes]    -- create full timeline
        tvMap = foldl insertMaybePair Map.empty (zip t1 v1)     -- create Map from ts1
        updatedMap = foldl insertMaybePair tvMap (zip t2 v2)    -- update the Map with ts2
        combinedValues = map (\t -> Map.lookup t updatedMap)    -- fill out times
                             completeTimes

-- Make TS an instance of Semigroup, with combineTS as <>
instance Semigroup (TS a) where
    (<>) = combineTS

-- Make TS an instance of Monoid
instance Monoid (TS a) where
    mempty = TS [] []

--
-- Analytics
--

-- Summary Statistics

-- Mean
mean :: (Real a) => [a] -> Double
mean xs = total / count
    where
        total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

-- Median
median :: (Real a) => [a] -> Double
median xs = if even len
            then avgMiddleTwo
            else realToFrac middle
    where
        len = length xs
        sorted = sort xs
        middleIndex = (length xs) `div` 2
        middle = head (drop middleIndex sorted)
        lowerMiddleIndex = middleIndex - 1
        avgMiddleTwo = mean $ take 2 (drop lowerMiddleIndex sorted)

-- Standard Deviation
std :: [Double] -> Double
std xs = sqrt . mean $ squaredDiffs
    where
        m = mean xs
        squaredDiffs = map (\x -> (x - m)^2) xs

-- Compute the mean of a time series, handling a TS with no values and a TS with all data missing
-- by returning Nothing. Hence meanTS has to return Maybe Double
meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS ts vs) = if all (== Nothing) vs
                    then Nothing
                    else Just avg
    where
        justVals = filter isJust vs             -- exclude missing data
        cleanVals = map fromJust justVals       -- remove the Maybe context
        avg = mean cleanVals      

-- Since we want to make other summary statistics, let's generalize and create a function that
-- enables summary statistics to operate on time series

type SummaryStat a = [a] -> Double
type TSSummaryStat a = TS a -> Maybe Double

-- Take a summary statistic function and use it to compute the statistic on a time series
-- Handle the case of an empty time series or series with all missing values
makeTSSummaryStat :: (Real a) => SummaryStat a -> TSSummaryStat a
makeTSSummaryStat func = newFunc
    where
        newFunc (TS _ []) = Nothing
        newFunc (TS ts vs) = if all (== Nothing) vs
                             then Nothing
                             else Just stat
            where
                justVals = filter isJust vs
                cleanVals = map fromJust justVals
                stat = func cleanVals

myMean = makeTSSummaryStat mean
myMedian = makeTSSummaryStat median
mySTD = makeTSSummaryStat std

-- Calculate max, min and other comparisons, in which we care about the time point as well as the value

-- typeclasses for function signatures
type CompareFunc a = a -> a -> a 
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

-- generate a time series comparision function from a comparision function
makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
    where
        newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
        newFunc (_, Nothing) (i, val) = (i, val)
        newFunc (i, val) (_, Nothing) = (i, val)
        newFunc (i1, Just val1) (i2, Just val2) =
                                            if func val1 val2 == val1
                                            then (i1, Just val1)
                                            else (i2, Just val2)

-- generic time series comparision function that appies one of the functions created
-- woth makeTSCompare
compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) =  if all (== Nothing) values
                                    then Nothing
                                    else Just best
    where
        pairs = zip times values
        best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

--
-- Transforming time series
--

-- Diff: Change in values over time

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair _ Nothing = Nothing
diffPair Nothing _ = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues)
    where
        shiftValues = tail values
        diffValues = zipWith diffPair shiftValues values

-- Smoothing

-- Moving Averages

-- meanMaybe computes mean of a list of Maybe a, by extracting the values from the Just elements
-- and returning Nothing if there are any Nothing elements in the list
meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals =    if any (== Nothing) vals
                    then Nothing
                    else (Just avg)
    where avg = mean (map fromJust vals)

-- movingAvg computes a list of averages using a sliding window of n
movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n =  if length nextVals == n
                    then meanMaybe nextVals:movingAvg restVals n
                    else []
    where
        nextVals = take n vals  -- the sliding window of n values starting at head of list
        restVals = tail vals    -- reset the slidng window to start at the tail of list
 
-- movingAverageTS computes the moving average of a time series using centering
-- Since there are n/2 missing values as the window slides (where n is the size of the window), we need to add
-- n `div` 2 Nothings to the list of moving averages. We want to center the data, so add these Nothigns to the beginning
-- and the end
movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
    where
        ma = movingAvg values n
        nothings = replicate (n `div` 2) Nothing
        smoothedValues = mconcat [nothings, ma, nothings]

-- Now let's generalize so we can use other measures of cemter (e.g., median) to compute moving averages

-- Define type for functions that measure the center of a distribution
type CenterMeasure a = [a] -> Double

-- centerMaybe computes a measure of center of a list of Maybe a, by extracting the values from the Just elements
-- and returning Nothing if there are any Nothing elements in the list
centerMaybe :: (Real a) => CenterMeasure a -> [Maybe a] -> Maybe Double
centerMaybe func vals =  if any (== Nothing) vals
                    then Nothing
                    else (Just avg)
    where avg = func (map fromJust vals)

-- movingCenter computes a list of "averages" using specified the center measure over a sliding window of n
movingCenter :: (Real a) => CenterMeasure a -> [Maybe a] -> Int -> [Maybe Double]
movingCenter func [] n = []
movingCenter func vals n =  if length nextVals == n
                            then thisCenter:movingCenter func restVals n
                            else []
    where
        nextVals = take n vals  -- the sliding window of n values starting at head of list
        restVals = tail vals    -- reset the slidng window to start at the tail of list
        thisCenter = centerMaybe func nextVals

movingCenterTS :: (Real a) => CenterMeasure a -> TS a -> Int -> TS Double
movingCenterTS func (TS [] []) n = TS [] []
movingCenterTS func (TS times values) n = TS times smoothedValues
    where
        ma = movingCenter func values n
        nothings = replicate (n `div` 2) Nothing
        smoothedValues = mconcat [nothings, ma, nothings]
--
-- Some Time Series data
--

-- Data files (since we don't know how to read in files yet)

file1 ::[(Int, Double)]
file1 = [ (1, 200.1), (2, 199.5), (3, 199.4)
        , (4, 198.9), (5, 199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9) ]

file2 :: [(Int, Double)]
file2 = [ (11, 201.6), (12, 201.5), (13, 201.5)
        , (14, 203.5), (15, 204.9), (16, 207.1)
        , (18, 210.5), (20, 208.8) ]

file3 :: [(Int, Double)]
file3 = [ (10, 201.2), (11, 201.6), (12, 201.5)
        , (13, 201.5), (14, 203.5), (17, 210.5)
        , (24, 215.1), (25, 218.7) ]

file4 :: [(Int, Double)]
file4 = [ (26, 219.8), (27, 220.5), (28, 223.8)
        , (29, 222.8), (30, 223.8), (31, 221.7)
        , (32, 222.3), (33, 220.8), (34, 219.4)
        , (35, 220.1), (36, 220.6) ]

-- Convert files to time series
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

-- Combine into a single time series
tsAll = mconcat [ts1, ts2, ts3, ts4]

