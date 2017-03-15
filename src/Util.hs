{-# LANGUAGE BangPatterns #-}

module Util
  (
    randomRoverDomain
  , repIO
  , randomN
  , best
  , bestLoyal
  , bestTraitor
  ) where

import System.Random
import qualified Data.Map.Strict as Map
import Data.List

import Models.POI
import Models.Rover
import Models.RoverDomain
import Sim.Simulation
import Models.State

import NN.NeuralNetwork
import CCEA

import Data.List (mapAccumL)                         

randomN :: RandomGen g => Int -> g -> (g -> Int -> (g,a)) -> (g, [a])
randomN n g f = mapAccumL (\g' x -> f g' x) g [1..n]

repIO  :: Int -> (Int -> IO a) -> IO [a]
repIO n f = sequence $ map (\x -> f x) [1..n]

randomPOI :: RandomGen g => (Int, Int) -> g -> Int -> (g, POI)
randomPOI (x,y) g uuid = let (g',rS) = getRandomState g (fromIntegral x, fromIntegral y)
                         in (g', POI rS 0 ((fromIntegral $ min x y) / 2) uuid Map.empty)

rRover :: (State -> (Network Double, NNVars) -> Int -> Rover) -> NNVars -> (Int,Int) -> Int -> IO Rover
rRover construct vars (i,j) uuid = do
  g <- newStdGen
  let (g', rS) = getRandomState g (fromIntegral i, fromIntegral j)
  net <- create vars
  return $! construct rS (net,vars) uuid 
  
randomTraitor :: NNVars -> (Int, Int) -> Int -> IO Rover
randomTraitor = rRover Traitor

randomLoyalist :: NNVars -> (Int, Int) -> Int -> IO Rover
randomLoyalist = rRover Rover

randomRoverDomain :: NNVars -> NNVars -> (Int, Int) -> Int -> Int -> Int -> IO (RoverDomain Rover POI)
randomRoverDomain varsL varsT b@(i,j) p a t = do
  g <- newStdGen
  let l@(x,y) = (i - 1, j - 1)
      (g', rPois) = randomN p g (randomPOI l)
  rRoverL <- repIO a (randomLoyalist varsL l)
  rRoverT <- repIO t (randomTraitor varsT l)
  return $! RoverDomain b (rRoverL ++ rRoverT) rPois

type Pop = Population (Network Double)

best :: ([Network Double] -> [Double]) -> Pop -> [Network Double]
best _ [] = []
best f p = let scores = map f p
               teamScore = map sum scores
               withScores = zip p teamScore
           in fst $ head $ sortOn snd withScores

bestLoyal :: RoverDomain Rover POI -> Int -> Pop -> IO [Network Double]
bestLoyal dom simSteps pop = do
  gen <- newStdGen
  let bNets = best (snd . gFit simSteps (gen,dom)) pop
  return bNets

bestTraitor :: RoverDomain Rover POI -> Int -> Pop -> IO [Network Double]
bestTraitor dom simSteps pop = do
  gen <- newStdGen
  let bNets = best (snd . traitorFit simSteps (gen, dom)) pop
  return bNets

