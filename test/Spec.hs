import Test.HUnit

import Models.LocationTest
import Models.OrientationTest
import Models.StateTest
import Models.POITest

allTests :: Test
allTests = TestList [ TestLabel "Models.Location Tests" testsLocation
                    , TestLabel "Models.Orientation Tests" testsOrientation
                    , TestLabel "Models.State Tests" testsState
                    , TestLabel "Models.POI Tests" testsPOI]

main :: IO Counts
main = runTestTT allTests
