{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

-- Capstone (Chapter 26) for Unit 4 - MARC Records

type Author = T.Text
type Title = T.Text
type Html = T.Text

data Book = Book {
      author :: Author
    , title :: Title
    }  deriving Show

bookToHtml :: Book -> Html
bookToHtml book =
    mconcat [ "<p>\n"
            , titleInTags
            , authorInTags
            , "</p>\n"
            ]
    where
        titleInTags = mconcat [ "<strong>", (title book), "</strong>\n" ]
        authorInTags = mconcat [ "<em>", (author book), "</em>\n"]


booksToHtml :: [Book] -> Html
booksToHtml books = mconcat [ "<html>\n"
                            , "<head><title>books</title>"
                            , "<meta charset='utf-8'/>"
                            , "</head>\n"
                            , "<body>\n"
                            , booksHtml
                            , "\n</body>\n"
                            , "</html>\n"
                            ]
    where
        booksHtml = (mconcat . (map bookToHtml)) books

main :: IO ()
main = do
    let myBooks = [book1, book2, book3]
    TIO.writeFile "books.html" (booksToHtml myBooks)
                            

-- Some sample data
book1 :: Book
book1 = Book {
                title = "The Conspiracy Against the Human Race"
              , author = "Ligotti, Thomas"
             }

book2 :: Book
book2 = Book {
                title = "A Short History of Decay"
              , author = "Cioran, Emil"
                
              }

book3 :: Book
book3 = Book {
                title = "Get Programming in Haskell"
              , author = "Kurt, Will"
             }
