-- Chaper 12 extended example of using types and records for patient information

-- Names

-- Define type synomnyms for comonents of a name
type FirstName = String
type MiddleName = String
type LastName = String

-- Define type for name
data Name = Name FirstName LastName
        | NameWithMiddle FirstName MiddleName LastName

-- Display names
showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

-- Define type for sex
data Sex = Male | Female deriving Show

-- Bloodtypes

-- Define the types, modeling Rh and ABO separately and creating a bloodtype type as a product
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

-- Display bloodtypes
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo' :: BloodType -> BloodType -> Bool
canDonateTo' (BloodType O _) _ = True    -- universal donor
canDonateTo' _ (BloodType AB _) = True   -- universal recipient
canDonateTo' (BloodType A _) (BloodType A _) = True
canDonateTo' (BloodType B _) (BloodType B _) = True
canDonateTo' _ _ = False

canDonateTo :: Patient -> Patient -> Bool
canDonateTo p1 p2 = canDonateTo' (bloodType p1) (bloodType p2)

-- Define patient using record syntax
data Patient = Patient  { name :: Name
                        , sex :: Sex
                        , age :: Int
                        , height :: Int
                        , weight :: Int
                        , bloodType :: BloodType
                        }


-- Create a patient summary
patientSummary :: Patient -> String
patientSummary p =
    outputLine stars ++
    outputLine ("Patient Name: " ++ showName (name p)) ++
    outputLine ("Sex: " ++ show (sex p)) ++
    outputLine ("Height: " ++ show (height p) ++ " in.") ++
    outputLine ("Weight: " ++ show (weight p) ++ " lb.") ++
    outputLine ("BloodType: " ++ showBloodType (bloodType p)) ++
    outputLine stars
    where
        stars = take 10 (repeat '*')
        outputLine l = l ++ "\n"

-- Displat the patient summary
printPatientSummary = putStrLn . patientSummary

-- SOme patients
jackieSmith :: Patient
jackieSmith = Patient   { name = Name "Jackie" "Smith"
                        , sex = Female
                        , age = 43
                        , height = 62
                        , weight = 115
                        , bloodType = BloodType O Neg }

fredJones :: Patient
fredJones = Patient     { name =  Name "Fred" "Jones"
                        , sex = Male
                        , age = 32
                        , height = 72
                        , weight = 174
                        , bloodType = BloodType AB Pos }

janeThomas :: Patient
janeThomas = Patient    { name = NameWithMiddle "Jane" "Susan" "Thomas"
                        , sex = Female
                        , age = 57
                        , height = 65
                        , weight = 156
                        , bloodType = BloodType A Neg }

samBrown :: Patient
samBrown = Patient      { name = NameWithMiddle "Sam" "Percy" "Brown"
                        , sex = Male
                        , age = 25
                        , height = 67
                        , weight = 162
                        , bloodType = BloodType B Pos }
