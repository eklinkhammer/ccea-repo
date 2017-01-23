module Lib
  (
    someFunc
  ) where

import AI.HNN.FF.Network
import Numeric.LinearAlgebra.HMatrix hiding (corr)

import NN.NeuralNetwork
import CCEA.CCEA

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Control.Monad.Reader

import System.Random

cceaVars :: CCEAVars
cceaVars = Map.fromList [("numberPools", 5)
                    ,("poolSize",4)
                    ,("percentChanceMaxFitnessIsChosen",0.85)
                    ,("numberGenerationsPerEpoch", 200)]

nnVars :: NNVars
nnVars = Map.fromList [("numberInputs", 2)
                      ,("numberHidden", 2)
                      ,("numberOutputs",1)
                      ,("timesToTrain",1)
                      ,("learningRate",0.85)
                      ,("percentHiddenToRandomize",0.4)
                      ,("percentOutputToRandomize",0.4)]

initState = 0 :: Double

expected = [0.0,1.0,1.0,0.0] :: [Double]

evaluator :: Double -> [Network Double] -> (Double, [Double])
evaluator _ nets = (0,map (\net -> let z1 = (get net $ fromList [0.0,0.0]) ! 0
                                       o1 = (get net $ fromList [1.0,0.0]) ! 0
                                       o2 = (get net $ fromList [0.0,1.0]) ! 0
                                       z2 = (get net $ fromList [0.0,0.0]) ! 0
                                       result = [z1,o1,o2,z2] :: [Double]
                                in sum $ map (\(a,b) -> (b - a) * (b - a)) $ zip result expected) nets)

createXorNetwork :: IO (CCEA (Network Double) Double)
createXorNetwork = createCCEA nnVars cceaVars initState evaluator

trainXorNetwork :: CCEA (Network Double) Double -> IO (CCEA (Network Double) Double)
trainXorNetwork ccea = do
  gen <- getStdGen
  let (trainedCCEA, _) = runEpoch cceaVars nnVars gen ccea
  return trainedCCEA


someFunc :: IO Double
someFunc = do
  ccea <- createXorNetwork
  trained <- trainXorNetwork ccea
  let (CCEA pop _ _) = trained
      network = pop !! 0 !! 0 :: Network Double
      response = output network sigmoid $ fromList [0.0,1.0]
      val = response ! 0 :: Double
  return val
