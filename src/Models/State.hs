module Models.State
  (
    moveState
  , State (..)
  , Cmd (..)
  , Location (..)
  , getQuad
  , getRandomState
  , stateDistance
  ) where

import Models.Location
import Models.Orientation
import System.Random

data State = State
  {
    _loc :: Location
  , _ori :: Orientation
  } deriving (Eq, Show)

data Cmd = Move Double Double
         | Turn Double
         | Stay


moveState :: State -> Cmd -> State
moveState state Stay = state
moveState (State loc ori) (Turn angle) = State loc (rotate ori angle)
moveState (State loc _)   (Move dx dy) = State newLoc newAngle
  where
    newLoc = addLoc loc (Location dx dy)
    newAngle = atan2 dy dx

-- get relative quadrant
-- Quadrants are shifted 45 degrees ccw (such that the first quadrant is the 90 degree
-- arc in front of state 1
getQuad :: State -> State -> Int
getQuad (State locx ori) (State locy _) = let diffV  = diffLoc locx locy
                                              angle  = atan2 (_y diffV) (_x diffV)
                                              -- 180 is pi instead of -pi
                                              ang    = if angle == pi then (-pi) else angle
                                              angle' = ang - ori + pi / 4
                                          in if angle' > pi / 2 then 2
                                             else if angle' < (-pi) / 2  then 3
                                                  else if angle' < pi / 2 && angle' > 0 then 1
                                                       else 4

getRandomState :: RandomGen g => g -> (Double, Double) -> (g,State)
getRandomState g (x,y)= let (i,g') = randomR (0,x) g
                            (j,g'') = randomR (0,y) g'
                            (t,g3) = randomR (0, 2 *pi) g''
                        in (g3, State (Location i j) t)
                              

stateDistance :: State -> State -> Double
stateDistance a b = distance (_loc a) (_loc b)
