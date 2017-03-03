module Models.RoverDomain
  (
    timestep
  , getGlobalScore
  , Scoring (..)
  , Agent (..)
  , Actor (..)
  , RoverDomain (..)
  , getAgents
  , getScoring
  , setAgents
  , setScoring
  , inBounds
  , reset
  ) where

import Models.State

import NN.NeuralNetwork
import qualified Data.Matrix as M
import System.Random
import Data.List (mapAccumL)

data RoverDomain a b = RoverDomain (Int,Int) [a] [b]

class Actor a where
  getState :: a -> State
  setState :: a -> State -> a
  getBoundingBox :: a -> [Location]
  getMove :: (Agent t1, Scoring t2) => RoverDomain t1 t2 -> a -> Cmd
  move :: a -> Cmd -> a
  oneMove :: (Agent t1, Scoring t2) => RoverDomain t1 t2 -> a -> a
  oneMove dom a = let cmd = getMove dom a
                  in move a cmd
  
class (Actor a) => Scoring a where
  getScore :: a -> Double
  setScore :: a -> Double -> a
  updateScore :: (Actor t) => RoverDomain t a -> a -> a
  
class (Actor a) => Agent a where
  setPolicy :: a -> (Network Double) -> a
  getFitness :: (Agent t1, Scoring t2) => RoverDomain t1 t2 -> a -> Double
  getPolicy :: a -> Network Double

  
timestep :: (Agent a, Scoring s) => RoverDomain a s -> RoverDomain a s
timestep domain = let a'     = map (oneMove domain) (getAgents domain)
                      p'     = map (oneMove domain) (getScoring domain)
                      newDom = setActors domain a' p'
                      p''    = map (updateScore newDom) p'
                  in setScoring newDom p''


getScoring :: RoverDomain t p -> [p]
getScoring (RoverDomain _ _ xs) = xs

setScoring :: RoverDomain t s -> [s] -> RoverDomain t s
setScoring (RoverDomain dim as _) xs = RoverDomain dim as xs

getAgents :: RoverDomain a t -> [a]
getAgents (RoverDomain _ xs _) = xs

setAgents :: RoverDomain a t -> [a] -> RoverDomain a t
setAgents (RoverDomain dim _ ps) xs = RoverDomain dim xs ps

setActors :: RoverDomain a t -> [a]  -> [t] -> RoverDomain a t
setActors (RoverDomain dim _ _) as ps = RoverDomain dim as ps

getGlobalScore :: Scoring s => RoverDomain t s -> Double
getGlobalScore = sum . map getScore . getScoring

instance (Show a, Show s, Actor a, Actor s) => Show (RoverDomain a s) where
  show = roverToString


roverToString :: (Show a, Show s, Actor a, Actor s) => RoverDomain a s -> String
roverToString (RoverDomain (h,w) xs ys) =
  let locStringXs = map intLocString xs
      locStringYs = map intLocString ys
      blankS  = M.fromList h w $ replicate (h*w) "_" :: M.Matrix String
      mat = foldl (\m (i,s) -> M.setElem s (toMatI i) m) blankS (locStringXs ++ locStringYs)
  in show mat

toMatI :: (Int,Int) -> (Int,Int)
toMatI (i,j) = (i+1, j+1)

locString :: (Show a, Actor a) => a -> (Location, String)
locString x = (_loc (getState x), show x)

roundLoc :: Location -> (Int, Int)
roundLoc (Location x y) = (round x, round y)

intLocString :: (Show a, Actor a) => a -> ((Int,Int), String)
intLocString x = let (l,s) = locString x in (roundLoc l, s)

inBounds :: RoverDomain a s -> State -> Bool
inBounds (RoverDomain (h,w) _ _) (State (Location h' w') _) = h' >= 0 && (round h') < h && w' >= 0 && (round w') < w

reset :: (Actor a, Scoring s, RandomGen g)
  => g -> RoverDomain a s -> (g, RoverDomain a s)
reset g dom = let withResetScores = resetScores dom
              in resetActors g withResetScores
                 
resetScores :: (Scoring s) => RoverDomain a s -> RoverDomain a s
resetScores dom = setScoring dom (map (\x -> setScore x 0) (getScoring dom))

resetActors :: (Actor a, Actor s, RandomGen g) => g -> RoverDomain a s -> (g, RoverDomain a s)
resetActors g dom@(RoverDomain (x,y) _ _) =
  let (g', newA)  = mapAccumL (resetState (x-1, y-1)) g  (getAgents dom)
      (g'', newP) = mapAccumL (resetState (x-1, y-1)) g' (getScoring dom)
  in (g'', setActors dom newA newP)

resetState :: (Actor a, RandomGen g) => (Int, Int) -> g -> a -> (g, a)
resetState b g a = let (g',s) = randomState b g in (g', setState a s)

randomLocation :: RandomGen g => (Int, Int) -> g -> (g, Location)
randomLocation (ii, ji) g = let (id, jd) = (fromIntegral ii, fromIntegral ji)
                                (x, g') = randomR (0, id) g
                                (y, g'') = randomR (0, jd) g'
                            in (g'', Location x y)

randomState :: RandomGen g => (Int, Int) -> g -> (g, State)
randomState b g = let (g', rLoc) = randomLocation b g in (g', State rLoc 0)
