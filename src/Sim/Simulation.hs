module Sim.Simulation
  (

  ) where

-- Can't seem to import CCEAFitnessFunction, or anything but CCEA (..) from CCEA.CCEA
-- All of the CCEA.EA stuff imports fine though ... ???
import CCEA
import NN.NeuralNetwork (NN (..))
import System.Random

test :: S -> [n] -> (S, [Double])
test = undefined

g :: BreedingStrategy n g
g = undefined

data S = S

z = RoverDomain [] []

nets = []
x :: (RandomGen g, NN n) => CCEA n RoverDomain g
x = CCEA nets (cceaFitnessFunction (\_ -> [])) g (tournament 2) z



data RoverDomain = RoverDomain POIs Agents

type Agents = [Agent]
type POIs   = [POI]

data Agent = Agent
data POI   = POI

class Actor a where
  getLoc :: a -> (Double, Double)
  getBoundingBox :: a -> [(Double, Double)]
  safeMove :: RoverDomain -> a -> a
  decideMove :: RoverDomain -> a -- Cmd

type ScoringFunction = (RoverDomain -> [Double])

assignNets :: NN n => RoverDomain -> [n] -> RoverDomain
assignNets = undefined

-- Wrap up these constants in a reader monad (or as args)
cceaFitnessFunction :: NN n
  => ScoringFunction -> RoverDomain -> [n] -> (RoverDomain, [Double])
cceaFitnessFunction f r ns = let withNets = assignNets r ns
                                 r' = simulation 25 withNets
                             in (r', f r')

simStep :: RoverDomain -> RoverDomain
simStep = undefined


simulation :: Int -> RoverDomain -> RoverDomain
simulation 0 r = r
simulation n r = let r' = simStep r in simulation (n-1) r'
