{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Models.RoverDomain
  (
    RoverDomain (..)
  , Actor (..)
  , Agent (..)
  , Scoring (..)
  , getAgents
  , getScoring
  , setAgents
  , setScoring
  , setActors
  , inBounds
  , reset
  , getGlobalScore
  , getGlobalScoreWithout
  , getAllScores
  , timestep
  ) where

import Models.State
import Models.Location
import Data.Maybe
import Data.List

import System.Random
import NN.NeuralNetwork
import qualified Data.Matrix as M


type UUID = Int

class (Show a) => Actor a where
  getState :: a -> State
  setState :: a -> State -> a
  getMove :: (Agent t1, Scoring t2) => RoverDomain t1 t2 -> a -> Cmd
  move :: a -> Cmd -> a
  oneMove ::(Agent t1, Scoring t2) => RoverDomain t1 t2 -> a -> a
  oneMove dom a = let cmd = getMove dom a in move a cmd
  resetActor :: RandomGen g => (Int, Int) -> g -> a -> (g,a)
  resetActors :: RandomGen g => (Int, Int) -> g -> [a] -> (g,[a])
  resetActors b g as = mapAccumL (resetActor b) g as
  getID :: a -> UUID

class (Actor a) => Scoring a where
  getScore :: a -> Double
  setScore :: a -> Double -> a
  evalScore :: (Agent t1, Scoring t2) => RoverDomain t1 t2 -> a -> a
  evalScoreWithout :: (Agent t1, Scoring t2) => RoverDomain t1 t2 -> t1 -> a -> Double

class (Actor a) => Agent a where
  setPolicy :: a -> Network Double -> a
  getPolicy :: a -> Network Double
  getFitness :: (Agent t1, Scoring t2) => RoverDomain t1 t2 -> a -> Double
  isLoyal :: a -> Bool

  
data RoverDomain a b = RoverDomain (Int, Int) [a] [b]

getAgents (RoverDomain _ as _) = as
getScoring (RoverDomain _ _ ss) = ss    
setAgents (RoverDomain b _ ss) as = RoverDomain b as ss
setScoring (RoverDomain b as _) ss = RoverDomain b as ss
mapScoring r f = setScoring r (map f (getScoring r))
mapAgents r f = setAgents r (map f (getAgents r))                        
getBounds (RoverDomain b _ _) = b
setActors (RoverDomain b _ _) as ss = RoverDomain b as ss

reset g r@(RoverDomain b as ss) = let (g1, as') = resetActors b g as
                                      (g2, ss') = resetActors b g1 ss
                                  in (g2, setActors r as' ss')
                                              
inBounds dom s = let (h,w) = getBounds dom
                     (Location w' h') = _loc s
                 in h' > 0 && (round h') < h && w' > 0 && (round w') < w


getGlobalScore :: (Agent a, Scoring s) => RoverDomain a s -> Double
getGlobalScore d = sum $ map (getScore . evalScore d) (getScoring d)

getGlobalScoreWithout :: (Agent a, Scoring s) => RoverDomain a s -> a -> Double
getGlobalScoreWithout d a = sum $ map (evalScoreWithout d a) (getScoring d)

getAllScores :: (Agent a, Scoring s) => RoverDomain a s -> [(Int, Double)]
getAllScores d = map (\s -> (getID s, getScore s)) (getScoring d)

timestep d = let d1 = mapAgents d (oneMove d)
             in mapScoring d1 (evalScore d1 . oneMove d1)
                                                     
instance (Actor a, Actor b) => Show (RoverDomain a b) where
  show = roverToString

roverToString (RoverDomain (h,w) x y) =
  let lStringA = map intLocString x
      lStringS = map intLocString y
      blanks = M.fromList h w $ replicate (h*w) "_"
      m = foldl (\m (i,s) -> M.setElem s (toMatI i) m) blanks (lStringA ++ lStringS)
  in show m

intLocString :: (Show a, Actor a) => a -> ((Int,Int), String)
intLocString x = let (l,s) = locString x in (roundLoc l, s)

toMatI :: (Int,Int) -> (Int,Int)
toMatI (i,j) = (i+1, j+1)

locString :: (Show a, Actor a) => a -> (Location, String)
locString x = (_loc (getState x), show x)

roundLoc :: Location -> (Int, Int)
roundLoc (Location x y) = (round x, round y)

