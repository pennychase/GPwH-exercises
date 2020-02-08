-- Unit 1 Capstone - Functional Object-Oriented Programming with Robots
-- Part II - Fighting Robots!

-- The original capstone used closures. Rewriting to use types and records

module Robots where

-- Types
type Name = String
type Attack = Int
type HealthPoints = Int

data Robot = Robot {
    name :: String
  , attack :: Attack
  , hp :: HealthPoints
  }

instance Show Robot where
  show r = name r ++ " attack:" ++ show (attack r) ++ " hp:" ++ show (hp r)

-- Functions

-- damage changes the HP value by the damage from an attack
damage :: Robot -> Attack -> Robot
damage aRobot attackDamage =
    aRobot { hp = (hp aRobot) - attackDamage }

-- fight
-- fight represents attacker attacking defender. attacker's attack strength
-- inflicts damage on defender
fight :: Robot -> Robot -> Robot
fight attacker defender =
  damage defender attackDamage
  where attackDamage =  if (hp attacker) > 10   -- is attacker strong enough to cause damage
                        then (attack attacker)  -- yes - inflict "attack" damage
                        else 0                  -- no - inflcits 0 damage

-- round of a fight in which robtos attack simultaneously
roundSim :: [Robot] -> [Robot]
roundSim robots =
  [ fight robotA robotB, fight robotB robotA]
  where
    robotA = robots !! 0
    robotB = robots !! 1

-- round of a fight in which robots attack sequentially
roundSeq :: [Robot] -> [Robot]
roundSeq robots =
  [ robotA', robotB' ]
  where
    robotA = robots !! 0
    robotB = robots !! 1
    robotB' = fight robotA robotB
    robotA' = fight robotB' robotA

-- tournament
-- Fight a specified number of rounds
-- Round is passed as an argument to allow different types
tournament :: ([Robot] -> [Robot]) -> Int -> Robot -> Robot -> [Robot]
tournament f n robotA robotB = iterate f [robotA, robotB] !! n

-- Winner outputs the results of a tournament
winner :: [Robot] -> String
winner robots =
  case compare (hp robotA) (hp robotB) of
    GT -> show robotA
    LT -> show robotB
    EQ -> "Tie!"
  where
    robotA = robots !! 0
    robotB = robots !! 1

-- Get life of all the robots
getLife rs = map hp rs

-- Data

-- Some robots
killerRobot = Robot { name = "Kill3r"
                    , attack = 25
                    , hp = 200
                  }
gentleGiant = Robot { name = "Mr. Friendly"
                    , attack = 10
                    , hp = 300
                  }
fastRobot = Robot { name = "Speedy"
                    , attack = 15
                    , hp = 40
                  }
slowRobot = Robot { name = "Slowpoke"
                    , attack = 20
                    , hp = 30
                  }


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

-- Partial application to have a robot fight a buch of robots at the same time
ninjaRobot = Robot {
                      name = "Ninja"
                    , attack = 20
                    , hp = 250
                  }

ninjaFight = fight ninjaRobot
results = map ninjaFight [slowRobot, fastRobot, gentleGiant, killerRobot]
-- in GHCI:
-- results
-- getLife results
