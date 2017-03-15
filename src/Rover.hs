{-# LANGUAGE BangPatterns #-}

module Rover
  (
    someFunc
  ) where

import Models.POI
import Models.RoverDomain
import Models.Rover
import Models.State
import Sim.Simulation
import Util

import CCEA
import NN.NeuralNetwork hiding (train)
import Numeric.LinearAlgebra.HMatrix hiding (corr)

import System.IO.Unsafe
import System.Random
import Control.Monad.Writer
import Data.List (sortOn, mapAccumL, sort)

type Bounds = (Int,Int)
type POICount = Int
type LoyalCount = Int
type TraitorCount = Int
type DomainInfo = (Bounds, POICount, LoyalCount, TraitorCount)
type Domain = RoverDomain Rover POI

-- Given a domain (which may or may not have other agents with neural nets), trains
-- a population of neural networks to either be loyal or disloyal
trainX ::  Domain -> Pop -> Int -> SimSteps -> Bool -> IO Pop
trainX domain nets n steps trainLoyal = do
  let fit = if trainLoyal then dFit steps else traitorFit steps
  gen <- newStdGen
  let ccea :: CCEA (Network Double) (StdGen, Domain) StdGen
      cceaBreeding = elitist (mutateWeights nnVars)
      ccea = CCEA nets fit cceaBreeding (tournament 2) (gen, domain)
      ccea' = evolveNCCEA n gen ccea
      finalNets = _pop (snd ccea')
  return $! finalNets

testPopWithT :: DomainInfo -> NNVars -> NNVars -> [Network Double] -> [Network Double] -> SimSteps -> IO Double
testPopWithT (b,p,l,t) varsL varsT loyal traitor n = do
  roverDom <- randomRoverDomain varsL varsT b p l t
  gen      <- newStdGen
  let roverDom' = assignNets (assignNetsT roverDom traitor) loyal
      afterSim  = simulation n roverDom'
  return $! getGlobalScore afterSim
              
type Pop = Population (Network Double)
type Team = [Network Double]
type Scores = [Double]
            

mean :: [Double] -> Double
mean !xs = sum xs / (fromIntegral (length xs))

stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ var xs

var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/(fromIntegral $ n + 1)
           
type NGens = Int
type NTrials = Int
type NExps = Int
type SimSteps = Int

-- Trains a population of neural networks in an adversarial setting against another team
-- of neural networks in some random (fixed throughout training) domain.
-- Then tests the best team across random domains.
-- Returns the population, the best team, and the scores of the best team
evalTrain :: NTrials -> DomainInfo -> Pop -> [Network Double] -> NNVars -> NNVars -> NGens -> SimSteps -> Bool -> IO (Pop, [Network Double], [Double])
evalTrain nTrials info@(b,p,l,t) pop bestOthers varsL varsT nGens steps isLoyal = do
  dom <- randomRoverDomain varsL varsT b p l t
  let (bestF, assignF) = if isLoyal then (bestLoyal, assignNetsT)
                                 else (bestTraitor, assignNets)
  let domWithOthers = assignF dom bestOthers
  trained <- trainX domWithOthers pop nGens steps isLoyal
  bestTrained <- bestF domWithOthers steps trained

  let (bestLoyal, bestTraitor) = if isLoyal then (bestTrained, bestOthers)
                                 else (bestOthers, bestTrained)
                                      
  scores <- repIO nTrials (\_ -> testPopWithT info varsL varsT bestLoyal bestTraitor steps) :: IO [Double]
  return $! (trained, bestTrained, scores)

-- Next step. Make pointfree.
repEvalTrain :: NExps -> IO (Pop, Team, Scores) -> IO (Pop, Team, Scores)
repEvalTrain n f = repIO n (\_ -> f) >>= return . last . sortOn (\(a,b,c) -> mean c)

repEvalTrainT n f = repIO n (\_ -> f) >>= return . head . sortOn (\(a,b,c) -> mean c)

getTrainingData :: NExps -> IO [(Pop, Team, Scores)] -> IO [(Double, Double)]
getTrainingData n fs = do
  allGens <- fs
  repEachGen <- sequence $ map (repEvalTrain n . return) allGens
  return $! map (\(a,b,c) -> (mean c, stddev c)) repEachGen


getTrainingDataT n fs = do
  allGens <- fs
  repEachGen <- sequence $ map (repEvalTrainT n . return) allGens
  return $! map (\(a,b,c) -> (mean c, stddev c)) repEachGen
  
      
someFunc :: IO ()
someFunc = do
  let numL = 2
      numT = 4
      numP = 10
      b    = (10,10)
      domInfo  = (b, numP, numL, 0)
      domInfo2 = (b, numP, numL, numT)
      nTrials  = 30
      nGens    = 1
      nExps    = 15
      simStep  = 15
      varsN  = nnVarsNaive
      vars   = nnVars

  gen   <- newStdGen

  -- Strong foundation (Pre trained loyal)
  loyalNaive <- createPopulation 10 numL nnVarsNaive :: IO Pop
  (ln1,_, _) <- repEvalTrain 20 (evalTrain nTrials domInfo loyalNaive [] varsN varsN nGens simStep True)
  (ln2,_, _) <- repEvalTrain 20 (evalTrain nTrials domInfo ln1 [] varsN varsN nGens simStep True)
  (ln3,_, _) <- repEvalTrain 20 (evalTrain nTrials domInfo ln2 [] varsN varsN nGens simStep True)

  
  -- Loyal Agents Trained With 8 Input Neural Networks
  -- Step 1: Train loyal with 8
  let tNL x = evalTrain nTrials domInfo ln1 [] varsN varsN x simStep True
  (lnp, lnt, lns) <- repEvalTrain 20 (tNL nGens)
  putStrLn "Loyal Agents"
  getTrainingData 10 (sequence $ map (\n -> tNL n) [1,10..nGens]) >>= splitP


  newTraitors <- createPopulation 5 numT vars :: IO Pop
  let tTNL x = evalTrain nTrials domInfo2 newTraitors lnt varsN vars x simStep False
  (tp1,tt1,ts1) <- repEvalTrainT 20 (tTNL nGens)
  (tp2,tt2,ts2) <- repEvalTrainT 20 (evalTrain nTrials domInfo2 tp1 lnt varsN vars nGens simStep False)
  (tp, tt, ts) <- repEvalTrainT 20 (evalTrain nTrials domInfo2 tp2 lnt varsN vars nGens simStep False)

  putStrLn "Treasonous agents training with static loyal agents"
  getTrainingDataT 10 (sequence $ map (\n -> tTNL n) [1,10..nGens]) >>= print
  
  -- Loyal Agents Trained With 8 Input Neural Networks with Traitors
  -- Step 1: Train agents with above step traitors
  let tTNL' x = evalTrain nTrials domInfo2 lnp tt varsN vars x simStep True
  (lnp', lnt', lns') <- repEvalTrain 20 (tTNL' nGens)

  putStrLn "Loyal Agents trained with traitors"
  getTrainingData 10 (sequence $ map (\n -> tTNL' n) [1,10..nGens]) >>= print

  putStrLn "Now Agents can recognize traitors"

  -- Strong foundation (Pre trained loyal with traitor awareness)
  l <- createPopulation 10 numL nnVars :: IO Pop
  (l1, _, _) <- repEvalTrain 20 (evalTrain nTrials domInfo l [] vars vars nGens simStep True)
  (l2, _, _) <- repEvalTrain 20 (evalTrain nTrials domInfo l1 [] vars vars nGens simStep True)
  (l3, _, _) <- repEvalTrain 20 (evalTrain nTrials domInfo l2 [] vars vars nGens simStep True)
  
  -- -- Loyal Agents Trained With 12 Input Neural Networks
  
  let tL x = evalTrain nTrials domInfo l3 [] nnVars nnVars x simStep True
  (lp, lt, ls) <-  repEvalTrain 20 (tL nGens)
  
  putStrLn "Loyal agents that can recognize traitors"
  getTrainingData 10 (sequence $ map (\n -> tL n) [1,10..nGens]) >>= print


  -- Loyal Agents Trained With 12 Input Neural Networks with Traitors
  let tTL x = evalTrain nTrials domInfo2 lp tt nnVars nnVars x simStep True
  (lp', lt', ls') <-  repEvalTrain 20 (tTL nGens)

  putStrLn "Loyal Agents that can recognize traitors with traitors"
  getTrainingData 20 (sequence $ map (\n -> tTL n) [1,10..nGens]) >>= print




splitP as = matlabPrint (map fst as) >> matlabPrint (map snd as)
matlabPrint :: (Show a) => [a] -> IO ()
matlabPrint !as = putStr "[" >> matlabPrintHelper as
  where
    matlabPrintHelper [] = putStrLn "];"
    matlabPrintHelper (a:as) = putStr ((show a) ++ " ") >> matlabPrintHelper as

  
  --lnData          <- getTrainingData 20 (sequence $ map (\n -> evalTrainX nTrials domInfo loyalNaive [] nnVarsNaive nnVarsNaive n simStep True) [0,10..nGens])

  --  putStrLn $ show lnData
  --putStr "x = "
  --matlabPrint [0,10..nGens]
  --putStr "ln = "
  --matlabPrint (map fst lnData)
  --putStr "lne = "
  --matlabPrint (map snd lnData)
  
  -- Traitors trained with 12 Input Neural Networks vs Loyal Agents with 8 Input
  -- Step 1: Train with agents in above step
