-- Lesson 4.1 Functions as argument

import Data.List

-- compareLastNames is a comparison function on names represented as a tuple of (firstname, lastname) that can be used
-- by sortBy. It sorts on the lastname and if there's a tie sorts on the firstname
compareLastNames :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareLastNames name1 name2 =
    let
        lastNameCmp = compare (snd name1) (snd name2)
    in 
        if lastNameCmp == EQ
        then compare (fst name1) (fst name2)
        else lastNameCmp

-- Example of use
-- names = [("Peter", "Hook"), ("Ian", "Curtis"), ("Stephen", "Morris"), ("Tim", "Hook"), ("Bernard", "Sumner")]
-- sortBy compareLastNames names