{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Models.Domain
  (
    Domain (..)
  , Agent (..)
  , Actor (..)
  , Scoring (..)
  ) where

import Models.State
import Models.Agent
import Models.POI
import Models.Location
import NN.NeuralNetwork

import System.Random
import Data.Maybe
import Data.List

class Domain d where
  getGlobalScore :: d -> Double
  timestep :: d -> d
  getAgents :: (Agent a) => d -> [a]
  getScoring :: (Scoring b) => d -> [b]
  getOtherActors :: (Actor a) => d -> [c]
  setAgents :: (Actor a) => d -> [a] -> d
  setScoring :: (Actor b) => d -> [b] -> d
  setOther :: (Actor c) => d -> [c] -> d
  setActors :: (Agent a, Scoring b, Actor c) => d -> [a] -> [b] -> [c] -> d
  mapActors :: (Domain d') => d -> (a -> e) -> d'
  mapScoring :: (Domain d') => d -> (b -> e) -> d'
  mapOther :: (Domain d') => d -> (c -> e) -> d'
  resetDomain :: RandomGen g => g -> d -> (g,d)
  inBounds :: d -> State -> Bool
  inBounds dom s = let b       = getBounds dom
                       (Location w' h') = _loc s
                   in case b of
                        (Just (h,w)) -> h' > 0 && (round h') < h && w' > 0 && (round w') < w
                        Nothing -> True                        
  getBounds :: d -> Maybe (Int,Int)


class Actor a where
  getState :: a -> State
  setState :: a -> State -> a
  getMove :: (Domain d) => d -> a -> Cmd
  move :: a -> Cmd -> a
  oneMove :: (Domain d) => d -> a -> a
  oneMove dom a = let cmd = getMove dom a in move a cmd
  resetActor :: RandomGen g => g -> a -> (g,a)
  resetActors :: RandomGen g => g -> [a] -> (g,[a])
  resetActors g as = mapAccumL resetActor g as

class (Actor a) => Scoring a where
  getScore :: a -> Double
  setScore :: a -> Double -> a
  updateScore :: (Domain d) => d -> a -> a
  updateScore dom a = let score = evalScore dom a in setScore a score
  evalScore :: (Domain d) => d -> a -> Double

class (Actor a) => Agent a where
  setPolicy :: (NN n) => a -> n -> a
  getPolicy :: (NN n) => a -> n
  getFitness :: (Domain d) => d -> a -> Double
