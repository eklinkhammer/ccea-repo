module Util.Vars
  (
    Vars
  , getVar
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

getVar :: String -> Vars -> Double
getVar name bindings = fromJust (Map.lookup name bindings)

type Vars = Map.Map String Double
