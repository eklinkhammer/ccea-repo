module Models.Agent
  (

  ) where

import Models.RoverDomain
import Models.State
import Models.Location

import NN.NeuralNetwork

import Numeric.LinearAlgebra.HMatrix (Vector)
-- 
data Rover = Rover State (Network Double)

instance Actor Rover where
  getState (Rover s _) = s
  setState (Rover _ n) s = Rover s n
  getBoundingBox = undefined -- no in point robot sims
  getMove = undefined
  move r@(Rover _ _) cmd = setState r (moveState (getState r) cmd)

instance Agent Rover where
  setPolicy (Rover s _) p = Rover s p
  getPolicy (Rover _ p) = p
  getFitness domain (Rover _ _) = getGlobalScore domain


getPolicyInput :: (Agent a, Scoring p) => RoverDomain a p -> a -> Vector Double
getPolicyInput = undefined
-- sum of each type in four quadrants

getRelativeQuad :: (Actor a) => a -> a -> Int
getRelativeQuad x y = let stateX = getState x
                          stateY = getState y
                      in getQuad stateX stateY


  -- difference in locations, as vector, then get angle with atan2 y' x'. Rotate angle by x's orientation backwards, then compare to sectors

getNextMove :: (Agent a, Scoring p) => NNVars -> RoverDomain a p -> a -> Cmd
getNextMove nnVars domain agent = toCmd $ get nnVars (getPolicy agent) (getPolicyInput domain agent)

toCmd :: Vector Double -> Cmd
toCmd = undefined
