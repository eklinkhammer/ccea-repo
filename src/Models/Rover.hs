{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
module Models.Rover
  (
    Rover (..)
  , getPolicyInput
  , nnVars
  ) where

import Models.RoverDomain
import Models.Location
import Models.State

import qualified Data.Map as Map
import Data.List (sortOn)
import Numeric.LinearAlgebra.HMatrix (Vector, fromList, (!))

import NN.NeuralNetwork

import System.IO.Unsafe

data Rover = Rover
             {
               _roverState :: State
             , _roverNet   :: (Network Double)
             , _roverUuid :: Int
             }
           | Traitor
             {
               _roverState :: State
             , _roverNet :: (Network Double)
             , _roverUuid :: Int
             }
             

nnVars :: NNVars
nnVars = Map.fromList [("numberInputs", 12)
                      ,("numberHidden", 13)
                      ,("numberOutputs", 2)
                      ,("timesToTrain", 3)
                      ,("learningRate", 0.8)
                      ,("sigmoidOrTanh", 0)
                      ,("randomLowerBound", (-10))
                      ,("randomUpperBound", 10)
                      ,("mutationRate", 0.1)
                      ,("gaussianStdDev",1)
                      ,("gaussianMean",0)]
         
instance Actor Rover where
  getState (Rover s _ _) = s
  getState (Traitor s _ _) = s
  setState (Rover _ n i) s = Rover s n i
  setState (Traitor _ n i) s = Traitor s n i
  getID (Rover _ _ i) = i
  getID (Traitor _ _ i) = i
  getMove = getNextMove nnVars
  move r cmd = setState r (moveState (getState r) cmd)
  resetActor (x,y) g r = let b = (fromIntegral x, fromIntegral y)
                             (g', s) = getRandomState g b
                         in (g', setState r s)
  

instance Agent Rover where
  setPolicy (Rover s _ i) n = Rover s n i
  setPolicy (Traitor s _ i) n = Traitor s n i
  getPolicy (Rover _ n _) = n
  getPolicy (Traitor _ n _) = n
  getFitness d r = let s = getGlobalScore d in if isLoyal r then s else (-1) * s
  isLoyal (Rover _ _ _) = True
  isLoyal (Traitor _ _ _) = False

instance Show Rover where
  show (Rover _ _ i) = "R" ++ (show i)
  show (Traitor _ _ i) = "T" ++ (show i)

instance Eq Rover where
  (==) r1 r2 = getID r1 == getID r2
getNextMove nVars domain agent =
  let cmd = toCmd $ get nVars (getPolicy agent) (getPolicyInput domain agent)
      currentState = getState agent
  in if inBounds domain (moveState currentState cmd) then cmd
     else getBackupMove domain currentState cmd

getPolicyInput dom a = let as = getState a
                           likeMe = if isLoyal a then isLoyal else not . isLoyal
                           agentStates = map getState $ filter likeMe (getAgents dom)
                           otherStates = map getState $ filter (not . likeMe) (getAgents dom)
                           scoreStates = map getState (getScoring dom)
                           actorStates = filter (/= as) agentStates

                           quadActor = map (getQuad as) actorStates
                           quadScore = map ((+) 4 . getQuad as) scoreStates
                           quadOther = map ((+) 8 . getQuad as) otherStates

                           actorSWithQuad = zip actorStates quadActor
                           scoreSWithQuad = zip scoreStates quadScore
                           otherSWithQuad = zip otherStates quadOther
                           all = actorSWithQuad ++ scoreSWithQuad ++ otherSWithQuad
                           
                           allOverR = map (\(a',q) -> (1.0 / (stateDistance as a'), q)) all
                           blanks = zip (replicate 12 0.0) [1,2,3,4,5,6,7,8,9,10,11,12]
                           allSorted = sortOn snd (allOverR ++ blanks)
                       in fromList $ map fst $ compress allSorted

-- given a list sorted by the second tuple value, if the second values are equal for
-- consecutive elements, sum the fst value
compress :: (Num a, Eq b) => [(a, b)] -> [(a, b)]
compress [] = []
compress (x:[]) = [x]
compress (x:y:zs) = if snd x == snd y then compress $ (fst x + fst y, snd x):zs
                    else x:(compress (y:zs))
                         
-- difference in locations, as vector, then get angle with atan2 y' x'.
-- Rotate angle by x's orientation backwards, then compare to sectors
getBackupMove dom s (Move dx dy) =
  if inBounds dom (moveState s (Move dx 0)) then Move dx 0
  else if inBounds dom (moveState s (Move 0 dy)) then Move 0 dy
       else Turn (pi / 2)
getBackupMove _ _ _ = Turn (pi / 2)

-- Assuming tanh activation function, output will be between -1 and 1
-- In that case, take values directly as x and y and normalize to unit vector
-- Test: Don't normalize
toCmd :: Vector Double -> Cmd
toCmd vec = let (x,y) = (vec ! 0, vec ! 1)
            in Move x y
            -- in if x == 0 && y == 0 then Move 0 0                                        
            --    else let s = abs x + abs y
            --         in Move (x / s) (y / s)
  --putStrLn $ show vec
  --putStrLn $ show move
--  return $! move