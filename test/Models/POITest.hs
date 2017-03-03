module Models.POITest
  (
    testsPOI
  ) where

import Test.HUnit hiding (State, Location)

import Models.POI
import Models.State
import Models.RoverDomain
import Models.Agent

testsPOI :: Test
testsPOI = TestList [ TestLabel "updateScore" testsUpdateScore]

testsUpdateScore :: Test
testsUpdateScore = TestList [ TestLabel "Update Score - Single Actor" testUpdateScore
                            , TestLabel "Update Score - No Update" testNoUpdate
                            , TestLabel "Update Score - Multiple Actors" testMultipleActors
                            , TestLabel "Update Score - Max Score" testMaxScore
                            , TestLabel "Update Score - Out of Range" testOutOfRange]

testUpdateScore :: Test
testUpdateScore = TestCase (do
                               let locP = Location 0 0
                                   locA = Location 3 4
                                   sP   = State locP 0
                                   sA   = State locA 0
                                   p    = POI sP 0 10
                                   a    = POI sA 0 10
                                   dom  = RoverDomain (10,10) [a] [p]
                                   p'   = updateScore dom p
                               assertEqual "Single Actor" 0.2 (getScore p'))

testNoUpdate :: Test
testNoUpdate = TestCase (do
                            let loc = Location 3 3
                                s   = State loc 0
                                p   = POI s 10 10
                                dom = RoverDomain (10,10) [] [p] :: RoverDomain Rover POI
                                p'  = updateScore dom p
                            assertEqual "No Update" (getScore p) (getScore p'))

testMultipleActors :: Test
testMultipleActors = TestCase (do
                                  let locP  = Location 0 0
                                      locA1 = Location 3 4
                                      locA2 = Location 1 0
                                      sP    = State locP 0
                                      sA1   = State locA1 0
                                      sA2   = State locA2 0
                                      p     = POI sP 0 10
                                      a1    = POI sA1 0 10
                                      a2    = POI sA2 0 10
                                      dom   = RoverDomain (10,10) [a1,a2] [p]
                                      p'    = updateScore dom p
                                  assertEqual "Multiple Actors" 1 (getScore p'))

testMaxScore :: Test
testMaxScore = TestCase (do
                            let locP = Location 0 0
                                sP   = State locP 0
                                p    = POI sP 0 10
                                dom  = RoverDomain (10, 10) [p] [p]
                                p'   = updateScore dom p
                            assertEqual "Score Upper Limit" 10 (getScore p'))

testOutOfRange :: Test
testOutOfRange = TestCase (do
                              let locP = Location 0 0
                                  sP   = State locP 0
                                  p    = POI sP 0 1
                                  locA = Location 10 10
                                  sA   = State locA 0
                                  pA   = POI sA 0 0
                                  dom  = RoverDomain (10,10) [pA] [p]
                                  p'   = updateScore dom p
                              assertEqual "No POIs in range" 0 (getScore p'))
