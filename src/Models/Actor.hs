module Models.Actor
  (

  ) where

import Models.State

class Actor a where
  getState :: a -> State
  move :: a -> Cmd -> a
  setState :: a -> State -> a
  toString :: a -> String
  nextMove :: a -> Cmd

instance Show (Actor a) where
  show = toString

instance (Agent a) => Actor a where
  move actor cmd = setState actor (moveState (getState actor) cmd)


instance Actor POI where
  move poi _ = poi


class Agent a where
  getInfo :: a -> Info
  makeDecision :: a -> Info -> (a, Cmd)
  receiveInfo :: a -> Info -> a
  getBroadcast :: a -> Info


instance Agent G where
  getInfo = -- state
  makeDecision = -- use neural net
  receiveInfo a _ = a -- ignore
  getBroadcast _ = () -- does not emit
