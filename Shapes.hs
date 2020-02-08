module Shapes where

type Radius = Double
type Height = Double
type Width = Double

data Shape =
      Circle Radius
    | Square Height
    | Rectangle Height Width
    deriving (Show, Eq)

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Square h) = h * h
area (Rectangle h w) = h * w

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * r * pi
perimeter (Square h) = 4 * h
perimeter (Rectangle h w) = 2 * h + 2 * w


