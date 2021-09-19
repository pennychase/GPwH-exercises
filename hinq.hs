import Control.Applicative
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

data Enrollment = Enrollment
    { student :: Int
    , course :: Int
    } deriving Show

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a) -- _select _join/data _where
                | HINQ_ (m a -> m b) (m a)             -- no _whhere clause

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True)) 

-- _select, _join, _where rewritten to take monads instead of lists
-- To support using the HINQ type for representing queries

-- Select property from values
_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals
    return (prop val)

-- Filter values
_where :: (Monad m, Alternative m) => (a ->Bool) -> m a -> m a
_where test vals = do
    val <- vals
    guard (test val)
    return val

-- Inner Join
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1, d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs

{-
-- Origial versions using lists
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
-}

-- Run a query using SQL order (select <elements> from <data> where <tests>)
-- Using lambda
_hinq selectQuery joinQuery whereQuery =
    (\joinData -> 
        (\whereResult ->
            selectQuery whereResult)
        (whereQuery joinData)
    ) joinQuery

-- Using let
_hinq' selectQuery joinQuery whereQuery =
    let
        joinData = joinQuery
        whereData = whereQuery joinData
    in  selectQuery whereData

-- Some properties to use in testing
startsWith :: Char -> String -> Bool
startsWith c str = c == head str

-- Example Data
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

enrollments :: [Enrollment]
enrollments = [ (Enrollment 1 101)
              , (Enrollment 2 101)
              , (Enrollment 2 201)
              , (Enrollment 3 101)
              , (Enrollment 4 201)
              , (Enrollment 4 101)
              , (Enrollment 5 101)
              , (Enrollment 6 201)
              ]

-- Example queries

-- Query without where using \_ -> True)
finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((=="English") . courseTitle . snd))

teacherFirstName :: [String ]
teacherFirstName = _hinq (_select firstName)
                         finalResult
                         (_where (\_ -> True))

-- Using HINQ with lists
query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((== "English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
              teachers

-- Using HINQ with Maybe
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

missingCourse :: Maybe Course
missingCourse = Nothing

-- Can use missingCourse to demonstrate what happens with missing data
-- Change _where clause to (== "English") to see what happens when _where fails
maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
              (_join possibleTeacher possibleCourse teacherId teacher)
              (_where ((== "French") . courseTitle . snd))

-- Enrollments and joins
studentEnrollmentsQ = HINQ_ (_select (\(st, en) ->
                                       (studentName st, course en)))
                            (_join students enrollments studentId student)

studentEnrollments = runHINQ studentEnrollmentsQ

studentCoursesQ = HINQ_ (_select (\(p, c) -> 
                                   (fst p, courseTitle c)))
                        (_join studentEnrollments courses snd courseId)

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
    where
        courseQuery = HINQ  (_select (fst . fst))
                            (_join studentEnrollments courses snd courseId)
                            (_where ((== courseName) . courseTitle . snd))



