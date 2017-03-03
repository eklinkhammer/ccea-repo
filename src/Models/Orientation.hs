module Models.Orientation
  (
    Orientation
  , rotate
  ) where

type Orientation = Double

doubleMod :: Double -> Double -> Double
doubleMod x y = let rounded :: Int
                    rounded = round (x / y)
                    quotient :: Double
                    quotient = fromIntegral rounded
                in x - (y * quotient)

clipAngle :: Orientation -> Orientation
clipAngle theta = if theta > 2 * pi then doubleMod theta (2*pi)
                  else if theta < 0
                       then 2*pi + (doubleMod theta (-2*pi))
                       else theta
                            
rotate :: Orientation -> Double -> Orientation
rotate angle by = let sumAngle = angle + by
                  in clipAngle sumAngle
