{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

-- Capstone (Chapter 26) for Unit 4 - MARC Records

-- Types

-- Books
type Author = T.Text
type Title = T.Text
type Html = T.Text

data Book = Book {
      author :: Author
    , title :: Title
    }  deriving Show

-- Marc Records
type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString
type FieldText = T.Text

data FieldMetadata = FieldMetadata  { tag           :: T.Text
                                    , fieldLength   :: Int
                                    , fieldStart    :: Int 
                                    } deriving Show


-- Global Variables
leaderLength :: Int
leaderLength = 24

dirEntryLength :: Int
dirEntryLength = 12

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

subtitleSubfield :: Char
subtitleSubfield = 'b'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

-- Read Marc Records

rawToInt :: B.ByteString -> Int
rawToInt = (read . T.unpack . E.decodeUtf8)

-- Read in Marc Records
getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
    where
        recordLength = (getRecordLength . getLeader) marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                        then []
                        else next : allRecords rest
    where (next, rest) = nextAndRest marcStream

-- Read the directory

-- The base address is where the base record begins. It is bytes 12-16 in the leader.
getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
    where
        remainder = B.drop 12 leader

-- The directory starts after the leader, so to find its size you subtract
-- the base address from the end of the leader (leaderLength + 1)
getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
    where
        directoryLength = (getDirectoryLength . getLeader) record
        afterLeader = B.drop leaderLength record

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =  if directory == B.empty
                            then []
                            else nextEntry : splitDirectory restEntries
    where
        (nextEntry, restEntries) = B.splitAt dirEntryLength directory

-- Process directoy entries and look up MARC fields

makeFieldMetaData :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetaData entry = FieldMetadata textTag theLength theStart
    where
        (theTag, rest) = B.splitAt 3 entry
        textTag = E.decodeUtf8 theTag
        (rawLength, rawStart) = B.splitAt 4 rest
        theLength = rawToInt rawLength
        theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata rawEntries = map makeFieldMetaData rawEntries

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
    where
        recordLength = getRecordLength record
        baseAddress = getBaseAddress record
        baseRecord = B.drop baseAddress record
        baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
        byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =   if length results < 1
                                    then Nothing
                                    else Just (head results)
    where
        metadata = (getFieldMetadata . splitDirectory . getDirectory) record
        results = filter ((== aTag) . tag) metadata

lookupSubfield :: (Maybe FieldMetadata) -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just fieldMetadata) subfield record =
        if results == []
        then Nothing
        else Just ((T.drop 1 . head) results)
    where
        rawField = getTextField record fieldMetadata
        subfields = T.split (== fieldDelimiter) rawField
        results = filter ((== subfield) . T.head) subfields


lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
    where
        entryMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupSubtitle :: MarcRecordRaw -> Maybe Title
lookupSubtitle = lookupValue titleTag subtitleSubfield

lookupFullTitle :: MarcRecordRaw -> Maybe Title
lookupFullTitle record = lookupTitle record <> lookupSubtitle record

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

-- Processing a stream of MARC records into HTML
marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
    where
        records = allRecords marcStream
        titles = map lookupFullTitle records
        authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs =
        map (\(title, author) -> Book   { title = fromJust title
                                        , author = fromJust author
                                        }) justPairs
    where
        justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . (take n) . marcToPairs

-- Generate HTML file of books

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
    marcData <- B.readFile "ohsu_ncnm_wscc_bibs.mrc"
    let marcRecords = processRecords 500 marcData
    TIO.writeFile "books.html" marcRecords

-- Test creating html file of books
main1 :: IO ()
main1 = do
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
