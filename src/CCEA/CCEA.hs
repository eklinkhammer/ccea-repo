module CCEA.CCEA
  (
    createCCEA
  , runEpoch
  , runGeneration
  , CCEAVars (..)
  , CCEA (..)
  ) where

import System.Random
import System.Random.Shuffle

import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.List
import Control.Monad.Reader


import Util.Vars
import NN.NeuralNetwork

type CCEAVars = Vars

data CCEA n s = CCEA (Population n) (CCEAEvaluator n s) s
type Population n = [[n]]
type Scores = [[Double]]
type CCEAEvaluator n s = (s -> [n] -> (s, [Double])) -- Refactor to State Monad

createCCEA :: (NN n) => NNVars -> CCEAVars -> s -> CCEAEvaluator n s -> IO (CCEA n s)
createCCEA netVars cceaVars state eval = runReader (createCCEAReader netVars eval state) cceaVars

createCCEAReader :: (NN n) => NNVars -> CCEAEvaluator n s -> s ->  Reader CCEAVars (IO (CCEA n s))
createCCEAReader netVars eval state = do
  numPools <- asks (getVar "numberPools")
  poolSize <- asks (getVar "poolSize")
  cceaVars <- ask
  let pools      = round numPools
      pool       = round poolSize
      networks   = createPopulation pools pool netVars
  return $ iotransform networks eval state
  
cullTheWeak :: (NN n, RandomGen g) => CCEAVars -> g -> CCEA n s -> Scores -> (CCEA n s, g)
cullTheWeak vars g ccea scores = runReader (cullTheWeakReader g ccea scores) vars

cullTheWeakReader :: (NN n, RandomGen g) => g -> CCEA n s -> Scores -> Reader CCEAVars ((CCEA n s), g)
cullTheWeakReader g (CCEA pop eval state) scores = do
  percentBest <- asks (getVar "percentChanceMaxFitnessIsChosen")
  let survivingPop = jointMap (cullThePool g percentBest) pop scores
  return $ (CCEA survivingPop eval state, g)

cullThePool :: (NN n, RandomGen g) => g -> Double -> [n] -> [Double] -> [n]
cullThePool g percent nets scores = survivors
  where
    (boolArray, g') = createBoolArray g (length nets) percent
    popAndScores    = zip nets scores
    sortedPop       = map fst $ sortOn snd popAndScores
    popAndBools     = zip boolArray sortedPop
    targetLength    = length nets `div` 2
    selected        = take targetLength $ map snd $ filter fst popAndBools
    notSelected     = map snd $ filter (not . fst) popAndBools
    survivors       = survivors ++ take (targetLength - length selected) notSelected
    
createSuccessors :: (NN n, RandomGen g) => CCEAVars -> NNVars -> g -> CCEA n s -> CCEA n s
createSuccessors vars nnVars g ccea = runReader (createSuccessorsReader nnVars g ccea) vars

createSuccessorsReader :: (NN n, RandomGen g) => NNVars -> g -> CCEA n s -> Reader CCEAVars (CCEA n s)
createSuccessorsReader vars g c@(CCEA pop e s) = do
  let withMutants = map (\pool -> concatMap (createMutant vars g) pool) pop
  return $ CCEA withMutants e s

createAndEvaluateTeams :: (NN n, RandomGen g) => g -> CCEA n s -> (CCEA n s, Scores)
createAndEvaluateTeams gen (CCEA pop eval state) = (CCEA shuffledPopulation eval finalState, correspondingScores)
  where
    shuffledPopulation   = map (\xs -> shuffle' xs (length xs) gen) pop
    teams                = transpose shuffledPopulation
    (finalState, scores) = evaluateTeams teams state eval
    correspondingScores  = transpose scores

runEpoch :: (NN n, RandomGen g) => CCEAVars -> NNVars -> g -> CCEA n s -> (CCEA n s, g)
runEpoch cceaVars nnVars g ccea = runReader (runEpochReader nnVars g ccea) cceaVars

runEpochReader :: (NN n, RandomGen g) => NNVars -> g -> CCEA n s -> Reader CCEAVars (CCEA n s, g)
runEpochReader nnVars g ccea = do
  numberGenerations <- asks (getVar "numberGenerationsPerEpoch")
  cceaVars          <- ask
  let numGens = round numberGenerations
  return $ runGenerations numGens cceaVars nnVars g ccea
  where
    runGenerations 0 ccVars nVars g' c                = (c, g)
    runGenerations n ccVars nVars g' c@(CCEA pop e s) = let (newCCEA, newG) = runGeneration ccVars nVars g' c
                                                in runGenerations (n-1) ccVars nVars newG newCCEA


runGeneration :: (NN n, RandomGen g) => CCEAVars -> NNVars -> g -> CCEA n s -> (CCEA n s, g)
runGeneration vars nnVars g ccea = (mostlyTheBest, g')
  where
    withMutants              = createSuccessors vars nnVars g ccea
    (cceaWithNewPop, scores) = createAndEvaluateTeams g ccea
    (mostlyTheBest, g')      = cullTheWeak vars g cceaWithNewPop scores
  
createPool :: NN n => Int -> NNVars -> IO [n]
createPool poolSize vars = sequence $ map (\_ -> create vars) [1..poolSize]

createPopulation :: NN n => Int -> Int -> NNVars -> IO (Population n)
createPopulation numPools poolSize vars = sequence $ map (\_ -> createPool poolSize vars) [1..numPools]

iotransform :: NN n => IO (Population n) -> CCEAEvaluator n s -> s -> IO (CCEA n s)
iotransform ioPop eval s = do
  pop <- ioPop
  return $ CCEA pop eval s

evaluateTeams :: (NN n) => [[n]] -> s -> CCEAEvaluator n s -> (s, Scores)
evaluateTeams [] s _           = (s, [[]])
evaluateTeams (team:teams) s f = let (newState, scores) = f s team
                                     (finalState, restScores) = evaluateTeams teams newState f
                                 in (finalState, scores : restScores)

createMutant :: (NN n, RandomGen g) => NNVars -> g -> n -> [n]
createMutant vars g net = [net, fst $ randomize g vars net] 

createBoolArray :: (RandomGen g) => g -> Int -> Double -> ([Bool], g)
createBoolArray g 0 percent = ([], g)
createBoolArray g count percent = let (val, g')  = random g
                                      (bools, gf) = createBoolArray g' (count - 1) percent
                                      bool = val < percent
                                  in (bool : bools, gf)

jointMap :: (a -> b -> c) -> [a] -> [b] -> [c]
jointMap _ []     _     = []
jointMap _ _      []    = []
jointMap f (x:xs) (y:ys)= (f x y) : (jointMap f xs ys)
