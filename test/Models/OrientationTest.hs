module Models.OrientationTest
  (
    testsOrientation
  ) where

import Test.HUnit
import Models.Orientation

testsOrientation :: Test
testsOrientation = TestList [ TestLabel "rotate" testsRotate]

testsRotate :: Test
testsRotate = TestList [ TestLabel "Positive within 2pi" testRotateWithin2Pi
                       , TestLabel "Positive past 2pi" testRotatePast2Pi
                       , TestLabel "Positive past 4pi" testRotatePast4Pi
                       , TestLabel "Negative within 2pi" testRotateBackWithin2Pi
                       , TestLabel "Negative past 2pi" testRotateBackPast2Pi
                       , TestLabel "Negative past 4pi" testRotateBackPast4Pi]

testRotateWithin2Pi :: Test
testRotateWithin2Pi = TestCase $ assertEqual "test1" (1.5*pi) (rotate pi (pi / 2))

testRotatePast2Pi :: Test
testRotatePast2Pi = TestCase $ assertEqual "test2" (0.5 * pi) (rotate pi (3 * pi / 2))

testRotatePast4Pi :: Test
testRotatePast4Pi = TestCase $ assertEqual "test3" pi (rotate pi (4*pi))
                                                      
testRotateBackWithin2Pi :: Test
testRotateBackWithin2Pi = TestCase $ assertEqual "test4" (pi/4) (rotate (pi/2) ((-pi)/4))

testRotateBackPast2Pi :: Test
testRotateBackPast2Pi = TestCase $ assertEqual "test5" (3*pi/2) (rotate 0 ((-5)*pi/2))

testRotateBackPast4Pi :: Test
testRotateBackPast4Pi = TestCase $ assertEqual "test6" (3*pi/2) (rotate (pi/2) ((-5)*pi))
