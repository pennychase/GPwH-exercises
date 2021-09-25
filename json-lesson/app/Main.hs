module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T
import GHC.Generics

-- Derive FromJSON and ToJSON instances
data Book = Book
            { title :: T.Text
            , author :: T.Text
            , year :: Int
            } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

-- can round trip: decode (encode book1) :: Maybe Book
book1 :: Book
book1 = Book { author = "Will Kurt"
             , title = "Get Programming with Haskell"
             , year = 2017
             }

jsonBook1 :: BC.ByteString
jsonBook1 = 
    "{\"author\":\"Emil Cioran\",\"title\":\"A Short History of Decay\",\"year\":1949}"


-- to decode: eitherDecode jsonBookErr :: Either String Book
-- Shows the error - missing author
jsonBookErr :: BC.ByteString
jsonBookErr = 
    "{\"writer\":\"Emil Cioran\",\"title\":\"A Short History of Decay\",\"year\":1949}"

-- Create FromJSON and ToJSON instances
sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\":123}"

data ErrorMessage = ErrorMessage
                    { message :: T.Text
                    , errorCode :: Int
                    } deriving Show

instance FromJSON ErrorMessage where
    parseJSON (Object v) =
        ErrorMessage <$> v .: "message"
                     <*> v .: "error"

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message errorCode) =
        object [ "message" .= message
               , "error" .= errorCode
               ]



main :: IO ()
main = print "hi"
