module Models.State
  (
    moveState
  , State (..)
  , Cmd (..)
  , getQuad
  ) where

import Models.Location
import Models.Orientation

data State = State Location Orientation

data Cmd = Move Double Double
         | Turn Double
         | Stay


moveState :: State -> Cmd -> State
moveState state Stay = state
moveState (State loc ori) (Turn angle) =  State loc (rotate ori angle)
moveState (State loc ori) (Move dx dy) = State newLoc newAngle
  where
    newLoc = addLoc loc (Location dx dy)
    newAngle = atan2 dy dx

-- get relative quadrant
-- Quadrants are shifted 45 degrees ccw (such that the first quadrant is the 90 degree
-- arc in front of state 1
getQuad :: State -> State -> Int
getQuad (State locx ori) (State locy _) = let diffV  = diffLoc locx locy
                                              angle  = atan2 (_y diffV) (_x diffV)
                                              angle' = angle - ori + pi / 4
                                          in if angle' < (-pi) / 2  then 3
                                             else if angle' > pi / 2 then 2
                                                  else if angle' < pi / 2 then 1
                                                       else 4
