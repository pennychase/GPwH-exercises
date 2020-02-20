type FirstName = String
type LastName = String
type MiddleName = String

data Name =
        Name FirstName LastName
    |   NameWithMiddle FirstName MiddleName LastName
    |   TwoInitialsWithLast Char Char LastName

instance Show Name where
    show (Name firstName lastName) = firstName ++ " " ++ lastName
    show (NameWithMiddle firstName middleName lastName) =  firstName ++ " " ++ middleName ++ " " ++ lastName
    show (TwoInitialsWithLast f m lastName) = (f:" ") ++ (m:" ") ++ lastName

data Author = Author Name

instance Show Author where
    show (Author name) = show name

data Artist = Person Name | Band String

instance Show Artist where
    show (Person name) = show name
    show (Band bandName) = bandName

data Creator = AuthorCreator Author | ArtistCreator Artist

instance Show Creator where
    show (AuthorCreator author) = show author
    show (ArtistCreator artist) = show artist

data Book = Book {
      author        :: Creator
    , isbn          :: String
    , bookTitle     :: String
    , bookYear      :: Int
    , bookPrice     :: Double
}

data VinylRecord = VinylRecord {
      artist         :: Creator
    , recordTitle    :: String
    , recordYear     :: Int
    , recordPrice    :: Double
}

data CollectibleToy = CollectibleToy {
      name          :: String
    , description   :: String
    , toyPrice      :: Double
}

data Pamphlet = Pamphlet {
      title                 :: String
    , pamphletDescription   :: String
    , contact               :: String
}

data StoreItem =
      BookItem Book
    | RecordItem VinylRecord
    | ToyItem CollectibleToy
    | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy (PamphletItem pamphlet) = show (contact pamphlet)
madeBy _ = "unknown"


-- Examples
b = Book { author = AuthorCreator (Author (Name "Jane" "Austen"))
            , isbn = "12345"
            , bookTitle = "Pride and Prejudice"
            , bookYear = 1816
            , bookPrice = 10.00
        }

p = Pamphlet { title = "National Parks"
                , pamphletDescription = "A listing of the national parks"
                , contact = "National Parks Service"}

r1 = VinylRecord { artist = ArtistCreator (Band "The Beatles")
                  , recordTitle = "Yellow Submarine"
                  , recordYear = 1969
                  , recordPrice = 12.99
                }

r2 = VinylRecord { artist = ArtistCreator (Person (Name "Joni" "Mitchell") )
                  , recordTitle = "Blue"
                  , recordYear = 1971
                  , recordPrice = 11.99
                }


store = [ BookItem b, PamphletItem p, RecordItem r1, RecordItem r2 ]
