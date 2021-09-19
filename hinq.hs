import Control.Monad


data Name = Name { firstName :: String
                 , lastName :: String
                 }

instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophomore | Junior | Senior
            deriving (Eq, Ord, Enum, Show)

data Student = Student
    { studentId :: Int
    , gradeLevel :: GradeLevel
    , studentName :: Name
    } deriving Show

data Teacher = Teacher
    { teacherId :: Int
    , teacherName :: Name
    } deriving Show

data Course = Course
    { courseId :: Int
    , courseTitle :: String
    , teacher :: Int
    } deriving Show

students :: [Student]
students = 
    [ Student 1 Senior (Name "Audre" "Lorde")
    , Student 2 Junior (Name "Leslie" "Silko")
    , Student 3 Freshman (Name "Judith" "Butler")
    , Student 4 Senior (Name "Guy" "Debord")
    , Student 5 Sophomore (Name "Jean" "Baudrillard")
    , Student 6 Junior (Name "Julia" "Kristeva")
    ]

teachers :: [Teacher]
teachers = [ Teacher 100 (Name "Simone" "De Beauvoir")
           , Teacher 200 (Name "Susan" "Sontag")
           , Teacher 220 (Name "Robert" "Lowell")
           ]

courses :: [Course]
courses = [ Course 101 "French" 100
          , Course 201 "English" 200
          , Course 202 "English" 220
          ]

-- Select property from values
_select :: (a -> b) -> [a] -> [b]
_select prop vals = do
    val <- vals
    return (prop val)

-- Filter values
_where :: (a ->Bool) -> [a] -> [a]
_where test vals = do
    val <- vals
    guard (test val)
    return val

-- Inner Join
_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1, d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs

-- Run a query using SQL order (select <elements> from <data> where <tests>)
_hinq selectQuery joinQuery whereQuery =
    (\joinData -> 
        (\whereResult ->
            selectQuery whereResult)
        (whereQuery joinData)
    ) joinQuery

_hinq' selectQuery joinQuery whereQuery =
    let
        joinData = joinQuery
        whereData = whereQuery joinData
    in  selectQuery whereData

-- Some properties to use in testing
startsWith :: Char -> String -> Bool
startsWith c str = c == head str

-- Query without where using \_ -> True)
finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((=="English") . courseTitle . snd))

teacherFirstName :: [String ]
teacherFirstName = _hinq (_select firstName)
                         finalResult
                         (_where (\_ -> True))