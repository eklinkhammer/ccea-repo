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

evolveXNIO :: RandomGen g
  => SimSteps -> Int -> g -> CCEA (Network Double) (g, RoverDomain Rover POI) g -> IO (g, CCEA (Network Double) (g, RoverDomain Rover POI) g)
evolveXNIO _ 0 g c = return (g,c)
evolveXNIO simSteps !n !g !c = do
  let (g', c') = evolveCCEA g c
  let dom = snd $ _state c'
      nets = _pop c'
      domBestNets = assignNets dom (best (snd . gFit simSteps (g, dom)) nets)
      (testDom, log) = runWriter $ simulationWriter 10 domBestNets
  putStrLn $ show (getGlobalScore testDom)
  evolveXNIO simSteps (n - 1) g' c'

trainingDom :: IO (RoverDomain Rover POI)
trainingDom = randomRoverDomain nnVars (5,5) 1 1 0
  
train :: RoverDomain Rover POI -> Int -> SimSteps -> IO (Population (Network Double))
train roverDom n simSteps = do
  nets <- createPopulation 10 5 nnVars :: IO (Population (Network Double))
  gen  <- newStdGen
  let ccea :: CCEA (Network Double) (StdGen, RoverDomain Rover POI) StdGen
      cceaBreeding = elitist (mutateWeights nnVars) :: BreedingStrategy (Network Double) StdGen
      ccea =  CCEA nets (dFit simSteps) cceaBreeding (tournament 2) (gen,roverDom)
  let ccea'= evolveNCCEA n gen ccea
      finalNets = _pop (snd ccea')
  return $! finalNets

type Domain = RoverDomain Rover POI

-- Given a domain (which may or may not have other agents with neural nets), trains
-- a population of neural networks to either be loyal or disloyal
trainX ::  Domain -> Pop -> Int -> SimSteps -> Bool -> IO Pop
trainX domain nets n steps trainLoyal = do
  let fit = if trainLoyal then dFit steps else dFitT steps
  gen <- newStdGen
  let ccea :: CCEA (Network Double) (StdGen, Domain) StdGen
      cceaBreeding = elitist (mutateWeights nnVars)
      ccea = CCEA nets fit cceaBreeding (tournament 2) (gen, domain)
      ccea' = evolveNCCEA n gen ccea
      finalNets = _pop (snd ccea')
  return $! finalNets
      
testPop :: DomainInfo -> Population (Network Double) -> SimSteps -> IO Double
testPop (b,p,l,t) pop n = do
  roverDom <- randomRoverDomain nnVars b p l t
  gen      <- newStdGen
  let scores = map (snd . gFit n (gen, roverDom)) pop
      teamScores = map sum scores
      bestScore = head $ sort teamScores
  return $! bestScore

testPopWithT :: DomainInfo -> [Network Double] -> [Network Double] -> SimSteps -> IO Double
testPopWithT (b,p,l,t) loyal traitor n = do
  roverDom <- randomRoverDomain nnVars b p l t
  gen      <- newStdGen
  let roverDom' = assignNets (assignNetsT roverDom traitor) loyal
      afterSim  = simulation n roverDom'
  return $ getGlobalScore afterSim
  
meanScore :: Int -> DomainInfo -> Population (Network Double) -> SimSteps -> IO Double
meanScore n info pop simSteps = repIO n (\_ -> testPop info pop simSteps) >>= return . mean

-- Returns the before and after scores
experiment :: Int -> Int -> DomainInfo -> SimSteps -> IO (Double, Double)
experiment nTrials nGens info@(b,p,l,t) simSteps = do
  dom <- randomRoverDomain nnVars b p l t
  nets <- createPopulation 10 5 nnVars :: IO Pop
  untrained <- trainX dom nets 0 simSteps True
  trained <- trainX dom nets nGens simSteps False
  before <- meanScore nTrials info untrained simSteps
  after <- meanScore nTrials info trained simSteps
  return (before, after)

best :: ([Network Double] -> [Double]) -> Pop -> [Network Double]
best _ [] = []
best f p = let scores = map f p
               teamScore = map sum scores
               withScores = zip p teamScore
           in fst $ head $ sortOn snd withScores
              
type Pop = Population (Network Double)

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral (length xs))

type NGens = Int
type NTrials = Int
type NExps = Int
type SimSteps = Int

trainingEfficacy :: NExps -> NTrials -> NGens -> DomainInfo -> SimSteps -> IO Double
trainingEfficacy nExps nTrials nGens info nSimSteps = do
  expResults <- repIO nExps (\_ -> experiment nTrials nGens info nSimSteps)
  return $ mean $ map (\(b,a) -> a / b) expResults

averageScore :: NExps -> NTrials -> NGens -> DomainInfo -> SimSteps -> IO Double
averageScore nExps nTrials nGens info nSimSteps = do
  expResults <- repIO nExps (\_ -> experiment nTrials nGens info nSimSteps)
  return $ mean $ map snd expResults
  
someFunc :: IO ()
someFunc = do
  let domInfo  = ((5,5), 5, 2, 0)
      domInfo2 = ((5,5), 5, 2, 2)
      nTrials  = 30
      nGens    = 1000
      nExps    = 15
      simStep  = 20
      
  nets  <- createPopulation 5 2 nnVars :: IO Pop
  netsT <- createPopulation 2 2 nnVars :: IO Pop
  dom   <- randomRoverDomain nnVars (5,5) 5 2 0
  domT  <- randomRoverDomain nnVars (5,5) 5 2 2
  gen   <- newStdGen


  
  trained <- trainX dom nets nGens simStep True
  
  let bestNet = best (snd . gFit simStep (gen, dom)) trained
      domWithBestLoyal = assignNets domT bestNet

  testPopWithT domInfo bestNet [] simStep >>= print


  
  trainedT <- trainX domWithBestLoyal netsT nGens simStep False

  let bestNetT = best (snd . traitorFit simStep (gen, domT)) trainedT
      domWithBestTraitor = assignNetsT domWithBestLoyal bestNetT

  testPopWithT domInfo2 bestNet bestNetT simStep >>= print
  


  
  trainedLoyal <- trainX domWithBestTraitor trained nGens simStep True

  let bestNetFinal = best (snd . gFit simStep (gen, domT)) trainedLoyal
      finalDom = assignNets domWithBestTraitor bestNetFinal

  testPopWithT domInfo2 bestNetFinal bestNetT simStep >>= print
