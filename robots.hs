-- Unit 1 Capstone - Functional Object-Oriented Programming with Robots
-- Part II - Fighting Robots!

module Robots where

-- robot constructor using a tuple to represent multiple attributes: name, attack strength, hit points
robot :: (a, b, c) -> ((a, b, c) -> t) -> t
robot (name, attack, hp) = \message -> message (name, attack, hp)


-- accessor functions

-- helper functions that pattern match against the tuple
name :: (a, b, c) -> a
name (n, _, _) = n

attack :: (a, b, c) -> b
attack (_, a, _) = a

hp :: (a, b, c) -> c
hp (_, _, h) = h

-- the getter functions
getName :: (((a, b, c) -> a) -> t) -> t
getName aRobot = aRobot name

getAttack :: (((a, b, c) -> b) -> t) -> t
getAttack aRobot = aRobot attack

getHP :: (((a, b, c) -> c) -> t) -> t
getHP aRobot = aRobot hp

-- the setter functions
setName :: (((a1, b, c) -> ((a2, b, c) -> t1) -> t1) -> t2) -> a2 -> t2
setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))

setAttack :: (((a, b1, c) -> ((a, b2, c) -> t1) -> t1) -> t2) -> b2 -> t2
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))

setHP :: (((a, b, c1) -> ((a, b, c2) -> t1) -> t1) -> t2) -> c2 -> t2
setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

printRobot aRobot =
    aRobot (\(n, a, h) ->   n ++
                            " attack:" ++ (show a) ++ 
                            " hp:" ++ (show h))

-- damage changes the HP value by the damage from an attack
damage aRobot attackDamage =
    aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

-- fight
-- fight represents aRobot attacking defender. aRobot's attack strength inflicts damage on defender
fight aRobot defender = damage defender attack
    where attack =  if getHP aRobot > 10    -- aRobot is too weak to attack
                    then getAttack aRobot
                    else 0

-- Get life of all the robots
getLife rs = map getHP rs


-- Some Robots
killerRobot = robot ("Kill3r", 25, 200)
gentleGiant = robot ("Mr. Fiendly", 10, 300)

fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke", 20, 30)

-- Some three round fights

-- gentleGiant and killerRobot
gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

-- fastRobot and slowRobot Fght 1 - simultaneous attacks
fastRobotRound1 = fight slowRobot fastRobot
slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound2 = fight slowRobotRound1 fastRobotRound1
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound3 = fight slowRobotRound2 fastRobotRound2
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2

-- fastRobot and slowRobot Fght 2 - reflect speed of fastRobot (fastRobot attack first and slowRobot is updated before it attacks)
-- Note that the actual order of execution doesn't matter
slowRobotRound1' = fight fastRobot slowRobot
fastRobotRound1' = fight slowRobotRound1' fastRobot
slowRobotRound2' = fight fastRobotRound1' slowRobotRound1'
fastRobotRound2' = fight slowRobotRound2' fastRobotRound1'
slowRobotRound3' = fight fastRobotRound2' slowRobotRound2'
fastRobotRound3' = fight slowRobotRound3' fastRobotRound2'

