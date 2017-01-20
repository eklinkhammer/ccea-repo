module Lib where

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

samples :: Samples Double
samples = [ fromList [0, 0] --> fromList [0]
          , fromList [0, 1] --> fromList [1]
          , fromList [1, 0] --> fromList [1]
          , fromList [1, 1] --> fromList [0] 
          ]
          
getM :: Network Double -> V.Vector (Matrix Double)
getM (Network matrices) = matrices

createNet :: Reader NNVars (IO (Network Double))
createNet = do
  return $ createNetwork 1 [2] 3

randomizeMatrix :: Matrix Double -> IO (Matrix Double)
randomizeMatrix mat = undefined

someFunc :: IO ()
someFunc = do
  --ccea <- createCCEA nnVars cceaVars initState evaluator :: IO (CCEA (Network Double) Double)
  gen <- getStdGen
  --myCCEA <- createCCEA nnVars cceaVars initState evaluator :: IO (CCEA (Network Double) Double)
  --let (trainedCCEA, _) = runEpoch cceaVars nnVars gen ccea
  --    (CCEA pop _ _) = trainedCCEA
  --    n = head $ head pop
  --mapM_ (print . output n tanh . fst) samples
  
  putStrLn "------------------------"
  
  --let matrixVector = getM n
  --putStrLn $ concatMap (\x -> show x ++ "\n") matrixVector
  --putStrLn $ show (matrixVector V.! 0)
