module Models.POI
  (
    POI (..)
  ) where

import Models.State
import Models.Location
import Models.RoverDomain
import Data.List (minimum)

type ScoringRadius = Double
data POI = POI State Double ScoringRadius deriving (Eq)

instance Actor POI where
  getState (POI s _ _)   = s
  setState (POI _ x r) s = POI s x r
  getBoundingBox _   = undefined
  getMove _ _ = Stay
  move a _ = a

instance Scoring POI where
  getScore (POI _ s _) = s
  updateScore = updatePOI
  setScore (POI s _ r) s' = POI s s' r

instance Show POI where
  show _ = "P"

type Score = Double

updatePOI :: (Actor a) => RoverDomain a b -> POI -> POI
updatePOI dom p@(POI _ x _) = let x' = calcScore dom p
                              in setScore p (max x x') 

calcScore :: (Actor a) => RoverDomain a b -> POI -> Score
calcScore (RoverDomain _ xs _) p =
  let l = _loc $ getState p
      distances = map (getDistance l) xs
      scoring   = let r = scoringDistance p in filter (\x -> x < r) distances
  in if null scoring then 0 else 1 / (max 0.1 $ minimum scoring)

getDistance :: Actor a => Location -> a -> Score
getDistance p a = distance p (_loc $ getState a)

scoringDistance :: POI -> ScoringRadius
scoringDistance (POI _ _ r) = r
