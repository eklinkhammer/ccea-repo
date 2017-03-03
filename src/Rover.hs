{-# LANGUAGE BangPatterns #-}

module Rover
  (
    someFunc
  , showSim
  ) where

import Models.Location
import Models.State
import Models.Agent
import Models.POI
import Models.RoverDomain
import qualified Models.Domain as D


import Sim.Simulation
import CCEA
import NN.NeuralNetwork

import System.IO.Unsafe
import System.Random

import Control.Monad.Writer
import Data.List (sortOn, mapAccumL)

randomN :: RandomGen g => Int -> g -> (g -> (g,a)) -> (g, [a])
randomN n g f = mapAccumL (\g' _ -> f g') g [1..n]

randomLocation :: RandomGen g => (Int, Int) -> g -> (g, Location)
randomLocation (ii, ji) g = let (id, jd) = (fromIntegral ii, fromIntegral ji)
                                (x, g') = randomR (0, id) g
                                (y, g'') = randomR (0, jd) g'
                            in (g'', Location x y)

randomState :: RandomGen g => (Int, Int) -> g -> (g, State)
randomState b g = let (g', rLoc) = randomLocation b g in (g', State rLoc 0)

randomPOI :: RandomGen g => (Int, Int) -> g -> (g, POI)
randomPOI b g = let (g',rLoc) = randomLocation b g
                in (g', POI (State rLoc 0) 0 15) --(fromIntegral $ max (fst b) (snd b)))

randomRover :: RandomGen g => NNVars -> (Int, Int) -> g -> IO (g, Rover)
randomRover vars b g = do
  let (g',rLoc) = randomLocation b g
      rState = State rLoc 0
  net <- create vars
  return $! (g', Rover rState net True)


randomRoverDomain :: RandomGen g => NNVars -> (Int, Int) -> g -> IO (RoverDomain Rover POI)
randomRoverDomain vars b@(i,j) g = do
  let (g', rPois) = randomN 2 g (randomPOI (i-1,j-1))
  (g'',rRover) <- randomRover vars (i-1,j-1) g'
  (_, rRover2) <- randomRover vars (i-1,j-1) g''
  return $! RoverDomain b [rRover, rRover2] rPois

showSim :: IO ()
showSim = do
  gen <- getStdGen
  dom <- randomRoverDomain nnVars (5,5) gen
  let (dom', log) = runWriter $ simulationWriter 20 dom
  putStrLn $ id $ log
  putStrLn $ show $ getGlobalScore dom'


evolveXNIO :: RandomGen g
  => Int -> g -> CCEA a (g, RoverDomain Rover POI) g -> IO (g, CCEA a (g, RoverDomain Rover POI) g)
evolveXNIO 0 g c = return (g,c)
evolveXNIO !n !g !c = do
  let (g', c') = evolveX g c
  putStrLn "SIMULATION START"
  let (dom, log) = runWriter $ simulationWriter 10 (snd $ reset g' $ snd $ _state c')
  putStrLn $ id log
  putStrLn $ "SIMULATION END " ++ (show (getGlobalScore dom))
  evolveXNIO (n - 1) g' c'
  
evolveXN :: RandomGen g => Int -> g -> CCEA a b g -> (g, CCEA a b g)
evolveXN 0 g c = (g,c)
evolveXN n g c = let (g', c') = evolveX g c in evolveXN (n - 1) g' c'

evolveX :: RandomGen g => g -> CCEA a b g -> (g, CCEA a b g)
evolveX g c@(CCEA p ff br sf _) = let (g',ccea') = evolveCCEA g c
                                 in (g', CCEA p ff br sf (_state ccea'))
someFunc :: IO ()
someFunc = do
  nets <- createPopulation 10 10 nnVars
  gen  <- getStdGen
  roverDom <- randomRoverDomain nnVars (5,5) gen
  putStrLn "Agents upon creation of roverDom"
  putStrLn $ show $ map getState (getAgents roverDom)
  let ccea :: CCEA (Network Double) (StdGen, RoverDomain Rover POI) StdGen
      ccea =  CCEA nets gFit (elitist (mutateWeights nnVars)) (fitnessProp) (gen,roverDom)
      (g6, c6)     = evolveXN 100 gen ccea
      (dom', log) = runWriter $ simulationWriter 10 (snd $ reset g6 $ snd $ _state c6)
  putStrLn $ id $ log
  putStrLn $ show $ getGlobalScore dom'
  putStrLn $ show $ map getState (getAgents dom')
