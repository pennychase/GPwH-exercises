-- Unit 1 Capstone - Functional Object-Oriented Programming with Robots

-- Part I - Modeling a cup

-- constructor for cup
cup flOz = \message -> message flOz

-- accessors
getOz aCup = aCup (\flOz -> flOz)

-- drink message
drink aCup ozDrank = 
    if ozDiff >= 0
        then cup ozDiff
        else cup 0
    where
        flOz = getOz aCup
        ozDiff = flOz - ozDrank

-- isEmpty message
isEmpty aCup = getOz aCup == 0

-- make a cup
coffeeCup = cup 12

-- Model taking multiple sips
afterManySips = foldl drink coffeeCup [1, 1, 1, 1]