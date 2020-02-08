-- Lesson 4.2 Returning Functions


-- addressLetter takes a name and a location and creates an address specific to the location's conventions
addressLetter :: (String, String) -> String -> String
addressLetter name location = locationFunction name
	where locationFunction = getLocationFunction location

-- getLocationFunction returns the LocationFunction for the city 
getLocationFunction :: String -> (String, String) -> String
getLocationFunction location =
	case location of
		"dc" -> dcOffice
		"ny" -> nyOffice
		"sf" -> sfOffice
		"reno" -> renoOffice
		_ -> (\name -> (fst name) ++ " " ++ (snd name))


-- Location Functions

sfOffice name =
	if lastName < "L"
	then nameText ++ " - PO Box 1234 - San Francisco, CA 94111"
	else nameText ++ " - O Box 1010 - San Francisco, CA 94109"
	where
		lastName = snd name
		nameText = (fst name) ++ " " ++ lastName

nyOffice name =
	nameText ++ " : PO Box 789 - New York, NY 10013"
	where
		nameText = (fst name) ++ " " ++ (snd name)

renoOffice name =
	nameText ++ " - PO Box 456 - Reno, NV 89523"
	where
		nameText = snd name

dcOffice name =
	nameText ++ ", Esq."
	where
		nameText == (fst name) ++ " " ++ (snd name) 
