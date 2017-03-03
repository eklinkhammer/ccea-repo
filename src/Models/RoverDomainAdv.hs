module Models.RoverDomainAdv
  (

  ) where

import Models.Domain

data RoverDomainAdv a b c = RoverDomain (Int, Int) a b c

instance (Agent a, Scoring b, Actor c) => Domain (RoverDomainAdv a b c) where
  getAgents = undefined
