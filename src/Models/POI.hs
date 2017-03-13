{-# LANGUAGE AllowAmbiguousTypes #-}

module Models.POI
  (
    POI (..)
  , printPOI
  ) where

import Models.State
import Models.Location
import Models.RoverDomain
import Data.List (minimum)
import qualified Data.Map.Strict as Map

type ScoringRadius = Double
type Score = Double
data POI = POI
           {
             _poiState :: State
           , _poiScore :: Score
           , _poiScoringRadius :: ScoringRadius
           , _poiUuid :: Int
           , _closestAgents :: Map.Map Int Double
           } deriving (Eq)

instance Actor POI where
  getState (POI s _ _ _ _)   = s
  setState (POI _ x r i m) s = POI s x r i m
  getID (POI _ _ _ i _) = i
  getMove _ _ = Stay
  move a _ = a
  resetActor (x,y) g (POI _ _ rad uuid _) = let b = (fromIntegral x, fromIntegral y)
                                                (g', s) = getRandomState g b
                                            in (g', POI s 0 rad uuid Map.empty)

instance Scoring POI where
  getScore (POI _ s _ _ _) = s
  setScore (POI s _ r i m) s' = POI s s' r i m
  evalScore d p = let agents   = filter isLoyal $ getAgents d
                      locPOI   = _loc $ getState p
                      distID   = map (\a -> (square $ getDistance locPOI a, getID a)) agents
                      newMap   = foldl updateMap (getMap p) distID
                      mDist    = maxDist newMap
                      val      = if mDist == 0 then 0 else 1.0 / mDist
                      newScore = min 10 $ max val (_poiScore p)
                  in setMap (setScore p newScore) newMap
  evalScoreWithout d a p = let mapWithout = Map.delete (getID a) (getMap p)
                               mDist = maxDist mapWithout
                           in if mDist == 0 then 0 else min 10 $ 1.0 / mDist
                               
                           
instance Show POI where
  show p = "P" ++ (show $ _poiUuid p)

getDistance :: Actor a => Location -> a -> Double
getDistance p a = distance p (_loc $ getState a)

scoringDistance :: POI -> ScoringRadius
scoringDistance (POI _ _ r _ _) = r

square :: Double -> Double
square x = x*x

printPOI :: POI -> String
printPOI (POI _ score _ uuid _) = (show uuid) ++ ": " ++ (show score)

updateMap :: Map.Map Int Double -> (Double, Int) -> Map.Map Int Double
updateMap m (dist, uuid) = Map.insertWith (\new old -> if new > old then new else old) uuid dist m


maxDist :: Map.Map Int Double -> Double
maxDist = Map.foldl max 0

setMap :: POI -> Map.Map Int Double -> POI
setMap (POI s x r i _) m = POI s x r i m

getMap :: POI -> Map.Map Int Double
getMap (POI _ _ _ _ m) = m
