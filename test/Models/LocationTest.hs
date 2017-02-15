module Models.LocationTest
  (
    testsLocation
  ) where

import Test.HUnit hiding (Location)
import Models.Location

testsLocation :: Test
testsLocation = TestList [ TestLabel "distance" testsDistance
                         , TestLabel "diffLoc" testsDiffLoc
                         , TestLabel "addLoc" testsAddLoc]

testsDistance :: Test
testsDistance = TestList [ TestLabel "Dist.0" testDistanceZero
                         , TestLabel "Dist.1" testDistanceDifferentQuad
                         , TestLabel "Dist.2" testDistanceLocs]

testsDiffLoc :: Test
testsDiffLoc = TestList [ TestLabel "Diff.0" testDiffBothPos
                        , TestLabel "Diff.1" testDiffBothNeg]

testsAddLoc :: Test
testsAddLoc = TestList [ TestLabel "Add.0" testAddLocIdentity
                       , TestLabel "Add.1" testAddLocComm
                       , TestLabel "Add.2" testAddLocAc
                       , TestLabel "Add.3" testAddLocValue]
testDistanceZero :: Test
testDistanceZero = TestCase (do
                                let l1 = Location 3.0 4.0
                                    l2 = Location 0.0 0.0
                                assertEqual "With Zero V" 5.0 (distance l1 l2))

testDistanceDifferentQuad :: Test
testDistanceDifferentQuad = TestCase (do
                                         let l1 = Location (-1.0) (3.0)
                                             l2 = Location (-1.0) (-4.0)
                                         assertEqual "Diff Quad" 7.0 (distance l1 l2))

testDistanceLocs :: Test
testDistanceLocs = TestCase (do
                                let l1 = Location 1.0 (-4)
                                    l2 = Location (-2) 0
                                assertEqual "Distance" 5.0 (distance l1 l2))

testDiffBothPos :: Test
testDiffBothPos = TestCase (do
                               let l1 = Location 1.0 2.0
                                   l2 = Location 3.0 4.0
                               assertEqual "Pos Diff" (Location 2.0 2.0) (diffLoc l1 l2)
                               assertEqual "Neg Diff" (Location (-2) (-2)) (diffLoc l2 l1))

testDiffBothNeg :: Test
testDiffBothNeg = TestCase (do
                               let l1 = Location (-1.0) (-2.0)
                                   l2 = Location (-3.0) (-4.0)
                               assertEqual "Pos Diff" (Location (-2) (-2)) (diffLoc l1 l2)
                               assertEqual "Neg Diff" (Location 2 2) (diffLoc l2 l1))

testAddLocIdentity :: Test
testAddLocIdentity = TestCase (do
                                 let l1 = Location 4.0 4.0
                                     l2 = Location 0 0
                                 assertEqual "Identity" l1 (addLoc l2 l1))

testAddLocComm :: Test
testAddLocComm = TestCase (do
                              let l1 = Location 2.0 5.0
                                  l2 = Location 4.0 (-1)
                              assertEqual "Commutative" (addLoc l1 l2) (addLoc l2 l1))

testAddLocAc :: Test
testAddLocAc = TestCase (do
                            let l1 = Location 2 3
                                l2 = Location (-8) (-3.6)
                                l3 = Location 1.2 (-6.2)
                            assertEqual "Associative" (addLoc (addLoc l1 l2) l3) (addLoc l1 (addLoc l2 l3)))

testAddLocValue :: Test
testAddLocValue = TestCase (do
                               let l1 = Location (-1) 4.2
                                   l2 = Location 5.1 4.6
                               assertEqual "Value" (Location 4.1 8.8) (addLoc l1 l2))
