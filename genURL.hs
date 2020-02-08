-- Lesson 5.2 Closures Example

-- getRequestURL builds a URL request string
getRequestURL :: String -> String -> String -> String -> String
getRequestURL host apiKey resource id =
    host ++ 
    "/" ++
    resource ++
    "/" ++
    id ++
    "?token=" ++
    apiKey

-- genHostRequestBuilder creates a closure that captures the host
genHostRequestBuilder :: String -> String -> String -> String -> String
genHostRequestBuilder host = (\apiKey resource id -> getRequestURL host apiKey resource id)

-- genApiRequestBuilder
genApiRequestBuilder :: (String -> String -> String -> String) -> String -> String -> String -> String
genApiRequestBuilder hostBuilder apiKey = 
    (\resource id ->
        hostBuilder apiKey resource id)

-- Example
-- First capture the host - this would be used by a development team working with the same server
exampleURLBuilder = genHostRequestBuilder "http://example.com"
-- Then capture the API key - this would be used by a developers working with one specific service
myExampleURLBuilder = genApiRequestBuilder exampleURLBuilder "137Hask311"
-- Then actually use the closures with different resources and ids
-- myExampleURLBuilder "book" "1234"






