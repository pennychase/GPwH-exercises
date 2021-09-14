import qualified Data.Map as M

-- Chapter 31

-- Pizza code

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter d = pi * (d/2)^2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas pizza1 pizza2 =
        if costP1 < costP2
        then pizza1
        else pizza2
    where
        costP1 = costPerInch pizza1
        costP2 = costPerInch pizza2

describePizza :: Pizza -> String
describePizza (size, cost) =
    "The " ++ show size ++ " inch pizza is cheaper at " ++ show costSqIn ++ " per square inch"
    where
        costSqIn = costPerInch (size, cost)

-- Original user input (in IO context)
main :: IO ()
main = do
    putStrLn "What is the size of pizza 1?"
    size1 <- getLine
    putStrLn "What is cost of pizza1?"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2?"
    size2 <- getLine
    putStrLn "What is cost of pizza2?"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn $ describePizza betterPizza

costData :: M.Map Int Double 
costData = M.fromList [(1, 18.0), (2, 16.0)]

sizeData :: M.Map Int Double
sizeData = M.fromList [(1, 20.0), (2, 15.0)]

-- Original Map (in Maybe context)
maybeMain :: Maybe String
maybeMain = do
    size1 <- M.lookup 1 sizeData
    cost1 <- M.lookup 1 costData
    size2 <- M.lookup 2 sizeData
    cost2 <- M.lookup 2 costData
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return $ describePizza betterPizza

-- Generalize to monad

comparePizzaPrice :: Monad m => m Pizza -> m String
comparePizzaPrice pizzas = do
    pizza1 <- pizzas
    pizza2 <- pizzas
    let betterPizza = comparePizzas pizza1 pizza2
    return $ describePizza betterPizza

-- For IO
-- call as: comparePizzaPrice getPizza
getPizzaIO :: IO Pizza
getPizzaIO = do
    putStrLn "What is the size of the pizza?"
    size <- getLine
    putStrLn "What is cost of the pizza?"
    cost <- getLine
    let pizza = (read size, read cost)
    return pizza

-- For Maybe
getPizzaMap :: Int -> Maybe Pizza
getPizzaMap n = do
    size <- M.lookup n sizeData
    cost <- M.lookup n costData
    return (size, cost)



