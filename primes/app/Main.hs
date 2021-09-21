module Main where

import System.Environment
import Primes

-- Extend the capstone of Unit 6 by adding a CLI
-- But use the error handling from Unit 7 whichh uses Either

primeTest :: IO ()
primeTest = do
    print "Enter a number to test for primaility: "
    n <- read <$> getLine
    let result = isPrime n
    case result of
        (Right True) -> print "It's prime"
        (Right False) -> print "It's composite"
        (Left primeError) -> print primeError

factorIntoPrimes :: IO ()
factorIntoPrimes = do
    print "Ener a number to factor into primes: "
    n <- read <$> getLine
    let result = primeFactors n
    case result of
        (Left primeError) -> print primeError
        (Right factors) -> print $ "The factors are: " ++ show factors
              
main :: IO ()
main = do
    args <- getArgs
    case (head args) of
        "-p" -> primeTest
        "-f" -> factorIntoPrimes
        otherwise -> putStrLn "Usage: primes-exe [-p | -f]"