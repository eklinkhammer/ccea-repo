module Models.RoverDomain
  (
    timestep
  , getGlobalScore
  , Scoring (..)
  , Agent (..)
  , Actor (..)
  , RoverDomain (..)
  , getAgents
  , getScoring
  ) where

import Models.State
import Models.Location

import NN.NeuralNetwork

data RoverDomain a b = RoverDomain [a] [b]

class Actor a where
  getState :: a -> State
  setState :: a -> State -> a
  getBoundingBox :: a -> [Location]
  getMove :: (Actor t1, Actor t2) => RoverDomain t1 t2 -> a -> Cmd
  move :: a -> Cmd -> a
  oneMove :: (Actor t1, Actor t2) => RoverDomain t1 t2 -> a -> a
  oneMove dom a = let cmd = getMove dom a
                  in move a cmd

class (Actor a) => Scoring a where
  getScore :: a -> Double
  updateScore :: RoverDomain t a -> a -> a

class (Actor a) => Agent a where
  setPolicy :: a -> (Network Double) -> a
  getFitness :: (Actor t1, Scoring t2) => RoverDomain t1 t2 -> a -> Double
  getPolicy :: a -> Network Double

timestep :: (Agent a, Scoring s) => RoverDomain a s -> RoverDomain a s
timestep domain = let a'  = map (oneMove domain) (getAgents domain)
                      p'  = map (oneMove domain) (getScoring domain)
                      p'' = map (updateScore (RoverDomain a' p')) p'
                  in RoverDomain a' p''


getScoring :: Scoring p => RoverDomain t p -> [p]
getScoring (RoverDomain _ xs) = xs

--setScoring :: Scoring s => RoverDomain t s -> [s] -> RoverDomain t s
--setScoring (RoverDomain as _) xs = RoverDomain as xs

getAgents :: Agent a => RoverDomain a t -> [a]
getAgents (RoverDomain xs _) = xs

--setAgents :: Agent a => RoverDomain a t -> [a] -> RoverDomain a t
--setAgents (RoverDomain _ ts) xs = RoverDomain xs ts

getGlobalScore :: Scoring s => RoverDomain t s -> Double
getGlobalScore = sum . map getScore . getScoring
