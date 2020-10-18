import Data.Char

theQuotes = [ "Hello World"
            , "Madam I'm Adam"
            , "Four score and twenty years ago"
            , "Ask what you can do for your country, not what your coiuntry can do for you"
            , "One small step for man, one giant step for mankind"
            ]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":_) = []
lookupQuote (x:xs) = quote:(lookupQuote xs)
    where
        quote = (theQuotes !! (read x - 1)) ++ "\nDo you want another? "

main :: IO ()
main = do
    userInput <- getContents
    mapM_ putStrLn $ lookupQuote (lines userInput)
