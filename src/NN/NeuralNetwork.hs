{-# LANGUAGE FlexibleInstances #-}
module NN.NeuralNetwork
  (
    NN (..)
  , NNVars (..)
  , createNetworkW
  ) where

import AI.HNN.FF.Network
import Numeric.LinearAlgebra.HMatrix hiding (corr)


import System.Random (RandomGen)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Control.Monad.Reader

import Matrix.State
import RandomUtil.Random
import RandomUtil.RandomMatrix
import Util.Vars


type NNVars = Vars

class NN n where
  create :: NNVars -> IO n
  
  get :: n -> Vector Double -> Vector Double
  
  train :: NNVars -> n -> Sample Double -> n
  
  randomize :: (RandomGen g) => g -> NNVars -> n -> (n, g)

instance NN (Network Double) where
  create vars                   = createNetworkW vars
  get net input                 = getNetwork net input
  train vars net (input,output) = trainNetwork vars net [(input,output)]
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
getNetwork net input = output net sigmoid input

trainNetwork :: NNVars -> Network Double -> Samples Double -> Network Double
trainNetwork vars net samples = runReader (trainNetworkReader net samples) vars

trainNetworkReader :: Network Double -> Samples Double -> Reader NNVars (Network Double)
trainNetworkReader net samples = do
  timesToTrain  <- asks (getVar "timesToTrain")
  learningRate  <- asks (getVar "learningRate")
  let network      = trainNTimes (round timesToTrain) learningRate sigmoid sigmoid' net samples :: Network Double
  return network

randomizeNetwork :: RandomGen g => g -> NNVars -> Network Double -> (Network Double, g)
randomizeNetwork g vars net = runReader (randomizeNetworkReader g net) vars

randomizeNetworkReader :: RandomGen g => g -> Network Double -> Reader NNVars (Network Double, g)
randomizeNetworkReader g net = do
  percentHiddenRandom <- asks (getVar "percentHiddenToRandomize")
  percentOutputRandom <- asks (getVar "percentOutputToRandomize")
  let matrices            = getMatrices net
      hidden              = V.toList $ V.init matrices
      sampleHidden        = head hidden
      numHiddenRandom     = round $ percentHiddenRandom * (fromIntegral $ (rows sampleHidden) * (cols sampleHidden))
      output              = V.last matrices
      numOutputRandom     = round $ percentOutputRandom * (fromIntegral $ (rows output) * (cols output)) :: Int
      
      (hidden', (g',_))   = randomMapTwoGens (randomizeNRandomMatrixElements numHiddenRandom) g g  hidden
      (output', (g'',_))  = randomizeNRandomMatrixElements numOutputRandom g' g' output
      weights             = V.fromList (hidden' ++ [output'])
      net'                = fromWeightMatrices weights
  return (net', g'')

getMatrices :: Network Double -> V.Vector (Matrix Double)
getMatrices (Network matrices) = matrices
