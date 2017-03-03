module Models.StateTest
  (
    testsState
  ) where

import Test.HUnit
import qualified Models.State as S
import Models.Orientation


testsState :: Test
testsState = TestList [ TestLabel "move" testsMoveState
                      , TestLabel "quadrant" testsGetQuad]

testsMoveState :: Test
testsMoveState = TestList [ TestLabel "Turn Command" testTurn
                          , TestLabel "Stay Command" testStay
                          , TestLabel "Move along orientation" testMoveForward
                          , TestLabel "Pivot and Move" testMove]

testsGetQuad :: Test
testsGetQuad = TestList [ TestLabel "Forward" testGetQuadForward
                        , TestLabel "Left" testGetQuadLeft
                        , TestLabel "Right" testGetQuadRight
                        , TestLabel "Behind" testGetQuadBehind
                        , TestLabel "Respects Rotation" testGetQuadWithOrientation]

testTurn :: Test
testTurn = TestCase (do
                        let loc = S.Location 3.0 3.0 :: S.Location
                            oriInit = (pi / 4) :: Orientation
                            oriFinal = rotate oriInit (2*pi)
                            cmd = S.Turn (2*pi)
                            stateInitial  = S.State loc oriInit
                            stateExpected = S.State loc oriFinal
                            stateFinal    = S.moveState stateInitial cmd
                        assertEqual "Turn Command" stateExpected stateFinal)

testStay :: Test
testStay = TestCase (do
                        let loc = S.Location 3.0 3.0
                            ori = (pi / 4)
                            cmd = S.Stay
                            stateInit = S.State loc ori
                            stateExpected = S.State loc ori
                            stateFinal = S.moveState stateInit cmd
                        assertEqual "Stay Command" stateExpected stateFinal)

testMoveForward :: Test
testMoveForward = TestCase (do
                               let locInit = S.Location 3.0 3.0
                                   locFinal = S.Location 3.0 4.0
                                   ori = (pi / 4)
                                   oriFinal = (pi / 2)
                                   cmd = S.Move 0.0 1.0
                                   stateInit = S.State locInit ori
                                   stateExpected = S.State locFinal oriFinal
                                   stateFinal = S.moveState stateInit cmd
                               assertEqual "Move Forward" stateExpected stateFinal)

testMove :: Test
testMove = TestCase (do
                        let locInit = S.Location 3.0 3.0
                            locFinal = S.Location 4.0 4.0
                            oriInit = (pi / 2)
                            oriFinal = (pi / 4)
                            cmd = S.Move 1.0 1.0
                            stateInit = S.State locInit oriInit
                            stateExpected = S.State locFinal oriFinal
                            stateFinal = S.moveState stateInit cmd
                        assertEqual "Pivot and Move" stateExpected stateFinal)


testGetQuadForward :: Test
testGetQuadForward = TestCase (do
                                  let origin = S.State (S.Location 0 0) 0
                                      other = S.State (S.Location 5 2) 0
                                  assertEqual "Test Quad Forward" 1 (S.getQuad origin other))


testGetQuadLeft :: Test
testGetQuadLeft = TestCase (do
                               let origin = S.State (S.Location 0 0) 0
                                   other = S.State (S.Location 0 2 ) 0
                               assertEqual "Test Quad Left" 2 (S.getQuad origin other))


testGetQuadBehind :: Test
testGetQuadBehind = TestCase (do
                                 let origin = S.State (S.Location 0 0) 0
                                     other = S.State (S.Location (-1) 0) 0
                                 assertEqual "Test Quad Behind" 3 (S.getQuad origin other))


testGetQuadRight :: Test
testGetQuadRight = TestCase (do
                                let origin = S.State (S.Location 0 0) 0
                                    other = S.State (S.Location 2 (-5)) 0
                                assertEqual "Test Quad Right" 4 (S.getQuad origin other))


testGetQuadWithOrientation :: Test
testGetQuadWithOrientation = TestCase (do
                                          let originForward = S.State (S.Location 0 0) (pi / 2)
                                              other = S.State (S.Location 2 5) 0
                                          assertEqual "Test Quad Forward" 1 (S.getQuad originForward other))


