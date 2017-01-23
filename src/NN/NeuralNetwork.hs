{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module NN.NeuralNetwork
  (
    NN (..)
  , NNVars
  , createNetworkW
  ) where

import AI.HNN.FF.Network
import Numeric.LinearAlgebra.HMatrix hiding (corr)


import System.Random (RandomGen)
import qualified Data.Vector as V
import Control.Monad.Reader

import RandomUtil.Random
import RandomUtil.RandomMatrix
import Util.Vars


type NNVars = Vars

class NN n where
  create :: NNVars -> IO n
  
  get :: n -> Vector Double -> Vector Double
  
  train :: NNVars -> n -> Sample Double -> n
  train vars net samp = trainV vars net [samp]
  trainV :: NNVars -> n -> Samples Double -> n
  randomize :: (RandomGen g) => g -> NNVars -> n -> (n, g)

instance NN (Network Double) where
  create vars                   = createNetworkW vars
  get net input                 = getNetwork net input
  trainV vars net io   = trainNetwork vars net io 
  randomize g vars net          = randomizeNetwork g vars net

createNetworkW :: NNVars -> IO (Network Double)
createNetworkW env = runReader createNetworkReader env

createNetworkReader :: Reader NNVars (IO (Network Double))
createNetworkReader = do
  numInput  <- asks (getVar "numberInputs")
  numHidden <- asks (getVar "numberHidden")
  numOutput <- asks (getVar "numberOutputs")
  return $ createNetwork (round numInput) [(round numHidden)] (round numOutput)

getNetwork :: Network Double -> Vector Double -> Vector Double
getNetwork net input = output net tanh input

trainNetwork :: NNVars -> Network Double -> Samples Double -> Network Double
trainNetwork vars net samples = runReader (trainNetworkReader net samples) vars

trainNetworkReader :: Network Double -> Samples Double -> Reader NNVars (Network Double)
trainNetworkReader net samples = do
  timesToTrain  <- asks (getVar "timesToTrain")
  learningRate  <- asks (getVar "learningRate")
  let network      = trainNTimes (round timesToTrain) learningRate tanh tanh' net samples :: Network Double
  return network

randomizeNetwork :: RandomGen g => g -> NNVars -> Network Double -> (Network Double, g)
randomizeNetwork g vars net = runReader (randomizeNetworkReader g net) vars

randomizeNetworkReader :: RandomGen g => g -> Network Double -> Reader NNVars (Network Double, g)
randomizeNetworkReader g net = do
  percentHiddenRandom <- asks (getVar "percentHiddenToRandomize")
  percentOutputRandom <- asks (getVar "percentOutputToRandomize")
  let netMatrix           = getMatrices net
      hidden              = V.toList $ V.init netMatrix
      sampleHidden        = head hidden
      numHiddenRandom     = round $ percentHiddenRandom * (fromIntegral $ (rows sampleHidden) * (cols sampleHidden))
      outputVal           = V.last netMatrix
      numOutputRandom     = round $ percentOutputRandom * (fromIntegral $ (rows outputVal) * (cols outputVal)) :: Int
      
      (hidden', (g',_))   = randomMapTwoGens (randomizeNRandomMatrixElements numHiddenRandom) g g  hidden
      (output', (g'',_))  = randomizeNRandomMatrixElements numOutputRandom g' g' outputVal
      weights             = V.fromList (hidden' ++ [output'])
      net'                = fromWeightMatrices weights
  return (net', g'')

getMatrices :: Network Double -> V.Vector (Matrix Double)
getMatrices (Network m) = m
