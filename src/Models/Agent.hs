module Models.Agent
  (
    Rover (..)
  , createRover
  , createRoverRandomStart
  , nnVars
  , getPolicyInputA'
  ) where

import Models.RoverDomain
import Models.State
import Models.Location

import NN.NeuralNetwork

import Numeric.LinearAlgebra.HMatrix (Vector, fromList, (!))

import Data.List (group, sort, sortOn)
import System.Random
import qualified Data.Map.Strict as Map

-- State of Rover, Policy, and Loyalty Status
data Rover = Rover State (Network Double) Bool

nnVars :: NNVars
nnVars = Map.fromList [("numberInputs", 8)
                      ,("numberHidden", 12)
                      ,("numberOutputs", 2)
                      ,("timesToTrain", 3)
                      ,("learningRate", 0.8)
                      ,("sigmoidOrTanh", 0)
                      ,("randomLowerBound", (-5))
                      ,("randomUpperBound", 5)
                      ,("mutationRate", 0.4)
                      ,("gaussianStdDev",1.5)
                      ,("gaussianMean",0)]

instance Actor Rover where
  getState (Rover s _ _) = s
  setState (Rover _ n l) s = Rover s n l
  getBoundingBox = undefined -- no in point robot sims
  getMove = getNextMove nnVars
  move r@(Rover _ _ _) cmd = setState r (moveState (getState r) cmd)

instance Agent Rover where
  setPolicy (Rover s _ l) p = Rover s p l
  getPolicy (Rover _ p _) = p
  getFitness domain (Rover _ _ _) = getGlobalScore domain

instance Show Rover where
  show _ = "R"

  
getPolicyInput :: (Agent a, Scoring s) => RoverDomain a s -> Rover -> Vector Double
getPolicyInput dom a = let as = getState a
                           quadActor = map (getQuad as . getState) (getAgents dom)
                           quadScore = map (\s -> 4 + (getQuad as (getState s))) (getScoring dom)
                           freq      = (map length . group . sort) (quadActor ++ quadScore ++ [1,2,3,4,5,6,7,8])
                       in fromList $ map (fromIntegral .  ((-) 1)) freq

-- Incorporate Distance, not just presence
getPolicyInput' :: (Actor a, Actor s) => RoverDomain a s -> Rover -> Vector Double
getPolicyInput' dom a = let as = getState a
                            actorStates = filter (/= as) $ map getState (getAgents dom)
                            scoreStates = map getState (getScoring dom)
                            quadActor = map (getQuad as) actorStates
                            quadScore = map (\s -> 4 + (getQuad as s)) scoreStates
                            actorSWithQuad = zip actorStates quadActor
                            scoreSWithQuad = zip scoreStates quadScore
                            all = actorSWithQuad ++ scoreSWithQuad
                            allOverR = map (\(a',q) -> (10.0 / (stateDistance as a'), q)) all
                            blanks = zip (replicate 8 0.0) [1,2,3,4,5,6,7,8]
                            allSorted = sortOn snd (allOverR ++ blanks)
                        in fromList $ map (min 10 . fst) $ compress allSorted


getPolicyInputA' :: (Actor a, Actor s, Actor b) => RoverDomain a s -> b -> Vector Double
getPolicyInputA' dom a = let as = getState a
                             actorStates = filter (/= as) $ map getState (getAgents dom)
                             scoreStates = map getState (getScoring dom)
                             quadActor = map (getQuad as) actorStates
                             quadScore = map (\s -> 4 + (getQuad as s)) scoreStates
                             actorSWithQuad = zip actorStates quadActor
                             scoreSWithQuad = zip scoreStates quadScore
                             all = actorSWithQuad ++ scoreSWithQuad
                             allOverR = map (\(a',q) -> (10.0 / (stateDistance as a'), q)) all
                             blanks = zip (replicate 8 0.0) [1,2,3,4,5,6,7,8]
                             allSorted = sortOn snd (allOverR ++ blanks)
                         in fromList $ map (min 10 . fst) $ compress allSorted

-- given a list sorted by the second tuple value, if the second values are equal for
-- consecutive elements, sum the fst value
compress :: (Num a, Eq b) => [(a, b)] -> [(a, b)]
compress [] = []
compress (x:[]) = [x]
compress (x:y:zs) = if snd x == snd y then compress $ (fst x + fst y, snd x):zs
                    else x:(compress (y:zs))
                         
  -- difference in locations, as vector, then get angle with atan2 y' x'. Rotate angle by x's orientation backwards, then compare to sectors

getNextMove :: (Agent a, Scoring p) => NNVars -> RoverDomain a p -> Rover -> Cmd
getNextMove nVars domain agent =
  let cmd = toCmd $ get nVars (getPolicy agent) (getPolicyInput' domain agent)
      currentState = getState agent
  in if inBounds domain (moveState currentState cmd) then cmd
     else getBackupMove domain currentState cmd

getBackupMove :: (Agent a, Scoring s) => RoverDomain a s -> State -> Cmd -> Cmd
getBackupMove dom s (Move dx dy) =
  if inBounds dom (moveState s (Move dx 0)) then Move dx 0
  else if inBounds dom (moveState s (Move 0 dy)) then Move 0 dy
       else Stay
getBackupMove _ _ _ = Stay




-- Assuming tanh activation function, output will be between -1 and 1
-- In that case, take values directly as x and y and normalize to unit vector
toCmd :: Vector Double -> Cmd
toCmd vec = let (x,y) = (vec ! 0, vec ! 1)
            in if x == 0 && y == 0 then Move 0 0                                        
               else let s = abs x + abs y
                    in Move (x / s) (y / s)
                       
createRover :: State -> Network Double -> Bool -> Rover
createRover = Rover

createRoverRandomStart :: RandomGen g => g -> (Double, Double) -> Network Double -> (g, Rover)
createRoverRandomStart g bounds net = let (g', rS) = getRandomState g bounds
                                      in (g', createRover rS net True)
