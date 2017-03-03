{-# LANGUAGE AllowAmbiguousTypes #-}

module Sim.Simulation
  (
    cceaFitnessFunction
  , scoringFuncG
  , gFit
  , simulationWriter
  , simStepWriter
  , assignNets
  ) where

-- Can't seem to import CCEAFitnessFunction, or anything but CCEA (..) from CCEA.CCEA
-- All of the CCEA.EA stuff imports fine though ... ???
import CCEA
import NN.NeuralNetwork (NN (..), Network)
import System.Random

import Models.RoverDomain
import Models.Agent
import Control.Monad.Writer

gFit :: (Agent a, Scoring s, RandomGen g)
  => CCEAState g a s -> [Network Double] -> (CCEAState g a s, [Double])
gFit = cceaFitnessFunction scoringFuncG


roverDom :: (Scoring s, Agent a) => RoverDomain a s
roverDom = RoverDomain (5,5) [] []

elitistGaussianNoise :: (RandomGen g, NN n) => BreedingStrategy n g
elitistGaussianNoise = elitist (mutateWeights nnVars)

type ScoringFunction a s = (RoverDomain a s -> [Double])

assignNets :: Agent a => RoverDomain a s -> [Network Double] -> RoverDomain a s
assignNets dom nets = let withNets = map assignNet (zip nets (getAgents dom))
                      in setAgents dom withNets

assignNet :: (Agent a) => (Network Double, a) -> a
assignNet (net, agent) = setPolicy agent net


type CCEAState g a s = (g, RoverDomain a s)  
-- Wrap up these constants in a reader monad (or as args)
--cceaFitnessFunction :: (Agent a, Scoring s)
--  => ScoringFunction a s -> RoverDomain a s -> [Network Double] -> (RoverDomain a s, [Double])
--cceaFitnessFunction f r ns = let withNets = assignNets r ns
  --                               r' = simulation 25 withNets
    --                         in (reset r', f r')

cceaFitnessFunction :: (Agent a, Scoring s, RandomGen g)
  => ScoringFunction a s -> CCEAState g a s -> [Network Double] -> (CCEAState g a s, [Double])
cceaFitnessFunction f (g,r) ns = let withNets = assignNets r ns
                                     r' = simulation 10 withNets
                                 in (reset g r', f r')
simStepWriter :: (Show a, Show s, Agent a, Scoring s)
  => RoverDomain a s -> Writer String (RoverDomain a s)
simStepWriter dom = do
  let agents = getAgents dom
  tell ((show dom) ++ "\n" ++ (concatMap (show . getState) agents) ++ "\n" ++ (concatMap (show . getPolicyInputA' dom) agents) ++ "\n")
  return (simStep dom)

simulationWriter :: (Agent a, Scoring s, Show a, Show s)
  => Int -> RoverDomain a s -> Writer String (RoverDomain a s)
simulationWriter 0 r = do
  tell ("End of simulation" ++ "\n" ++ (show r))
  return r
simulationWriter n r = do
  tell ("Simulation - Steps to Go: " ++ (show n) ++ "\n")
  simStepWriter r >>= simulationWriter (n-1)
  
simStep :: (Agent a, Scoring s) => RoverDomain a s -> RoverDomain a s
simStep dom = let newAgents = map (oneMove dom) (getAgents dom)
                  movedDom  = setAgents dom newAgents
                  newScoring = map (updateScore movedDom) (getScoring movedDom)
              in setScoring movedDom newScoring


simulation :: (Agent a, Scoring s) => Int -> RoverDomain a s -> RoverDomain a s
simulation 0 r = r
simulation n r = let r' = simStep r in simulation (n-1) r'


scoringFuncG :: Scoring s => ScoringFunction a s
scoringFuncG dom = let agents = getAgents dom
                   in replicate (length agents) (getGlobalScore dom)
                         
                         
