module Models.Orientation
  (
    Orientation
  , rotate
  ) where

type Orientation = Double

doubleMod :: Double -> Double -> Double
doubleMod x y = let quot = fromIntegral $ round (x / y) :: Double
                in x - (y * quot)

rotate :: Orientation -> Double -> Orientation
rotate angle by = let sumAngle = angle + by
                  in if sumAngle > 2 * pi
                     then doubleMod sumAngle (2*pi)
                     else sumAngle
