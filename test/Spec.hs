import Test.HUnit

import Models.LocationTest

allTests :: Test
allTests = TestList [ TestLabel "Model.Location Tests" testsLocation]

main :: IO Counts
main = runTestTT allTests
