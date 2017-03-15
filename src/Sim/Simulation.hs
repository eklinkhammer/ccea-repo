{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Sim.Simulation
  (
    gFit
  , dFit
  , traitorFit
  , simulation
  , simulationWriter
  , simStepWriter
  , assignNets
  , assignNetsT
  ) where

import CCEA
import NN.NeuralNetwork (NN (..), Network)
import System.Random

import Control.Monad.Writer
import Models.RoverDomain
import Models.Rover
import Models.POI
import Models.State

import Data.List
import System.IO.Unsafe


gFit :: (RandomGen g, Agent a, Scoring b)
  => Int -> (g, RoverDomain a b) -> [Network Double] -> ((g, RoverDomain a b), [Double])
gFit n = cceaFitnessFunction n scoringFuncG

dFit :: (RandomGen g, Agent a, Scoring b)
  => Int -> (g, RoverDomain a b) -> [Network Double] -> ((g, RoverDomain a b), [Double])
dFit n = cceaFitnessFunction n scoringFuncD

traitorFit :: (RandomGen g, Agent a, Scoring b)
  => Int -> (g, RoverDomain a b) -> [Network Double] -> ((g, RoverDomain a b), [Double])
traitorFit n = cceaFitnessFunctionT n scoringFuncGT

elitistGaussianNoise :: (RandomGen g, NN n) => BreedingStrategy n g
elitistGaussianNoise = elitist (mutateWeights nnVars)

type ScoringFunction d = (d -> [Double])

assignNets' loyalty d nets = let (loyal, dis) = partition isLoyal (getAgents d)
                             in if loyalty then let withNets = map assignNet (zip nets loyal)
                                                in setAgents d (withNets ++ dis)
                                else let withNets = map assignNet (zip nets dis)
                                     in setAgents d (withNets ++ loyal)
                                       
assignNets d nets = assignNets' True d nets

assignNetsT d nets = assignNets' False d nets

assignNet (net, agent) = setPolicy agent net


type CCEAState g d = (g, d)  

cceaFitnessFunction n f s@(g,d) nets = let withNets = assignNets d nets
                                           d' = simulation n withNets
                                           (g',_) = split g
                                        in ((g',d), f d')
                                      

cceaFitnessFunctionT n f s@(g,d) nets = let withNets = assignNetsT d nets
                                            d' = simulation n withNets
                                            (g',_) = split g
                                        in ((g',d), f d')
                                      
simStepWriter dom = do
  let agents = getAgents dom
  tell ((show dom) ++ "\n\n")-- ++ (concatMap (show . getState) agents) ++ "\n" ++ (concatMap (show . getPolicyInput dom) agents) ++ "\n")
  return (timestep dom)

simulationWriter 0 d = do
  tell ("End of simulation" ++ "\n" ++ (show d))
  return d
simulationWriter n d = do
  let agent = head $ getAgents d
  tell ("Simulation - Steps to Go: " ++ (show n) ++ "\n")
  --tell ("POIs: " ++ (id (concatMap ((++) "\n" . show . getState) (getScoring d))) ++ "\n")
  --tell ("States: " ++ (id (concatMap ((++) "\n" . show . getState) (getAgents d))) ++ "\n")
  let distances = map (\p -> let l = _loc $ getState p
                             in map (\a -> getDistance l a) (getAgents d)) (getScoring d)
      scores    = map (getScore . evalScore d) (getScoring d)
                  
  tell ("Distances: " ++ (show distances) ++ "\n")
  
  --tell ("Policy Input: " ++ (id (concatMap ((++) "\n" . show . getPolicyInput d) (getAgents d))) ++ "\n")
  tell ("Scores: " ++ (show scores) ++ "\n")
  --tell ("Score: " ++ (show $ getAllScores d) ++ "\n")
  tell ("Total Score: " ++ (show $ getGlobalScore d) ++ "\n")
  
  simStepWriter d >>= simulationWriter (n-1)

simulation 0 d = d
simulation n d = let d' = timestep d in simulation (n-1) d'

scoringFuncG dom = replicate ((length . filter isLoyal . getAgents) dom) (getGlobalScore dom)

scoringFuncGT dom = replicate ((length . filter (not . isLoyal) . getAgents) dom) ( (-1) * (getGlobalScore dom))
                         
                         
scoringFuncD dom =
  let g = getGlobalScore dom
      agents = filter isLoyal (getAgents dom)
      ds = map (\a -> g - (getGlobalScoreWithout dom a)) agents
  in ds--  unsafePerformIO $ do
    -- if sum ds /= 0 then putStrLn $ show ds else putStrLn $ (show g)
    -- return $! ds
     
getScoreWithoutAgent dom agents a = 
  let withoutA     = delete a agents
      scoreWithout = getGlobalScoreWithout dom a
  in scoreWithout
  -- putStrLn $ "Score without agent " ++ (show a) ++ ": " ++ (show scoreWithout)
  -- putStrLn $ "Agents: " ++ (show agents)
  -- putStrLn $ "WithoutA: " ++ (show withoutA)
  -- putStrLn $ show dom
  -- putStrLn $ show newDom
  -- return $! scoreWithout
