module Models.Location
  (
    Location (..)
  , distance
  , addLoc
  , diffLoc
  ) where



data Location = Location
  {
    _x :: Double
  , _y :: Double
  } deriving (Eq, Show)
  
distance :: Location -> Location -> Double
distance l1 l2 = sqrt $ square (_x ld) + square (_y ld)
  where
    square n = n*n
    ld = diffLoc l1 l2

-- diff a b = b - a
diffLoc :: Location -> Location -> Location
diffLoc l1 l2 = locMath (-) l2 l1

addLoc :: Location -> Location -> Location
addLoc l1 l2 = locMath (+) l1 l2

locMath :: (Double -> Double -> Double) -> Location -> Location -> Location
locMath f l1 l2 = Location (f (_x l1) (_x l2)) (f (_y l1) (_y l2)) 
