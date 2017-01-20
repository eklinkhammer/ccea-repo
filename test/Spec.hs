import Test.HUnit
import Matrix.StateTest
import RandomUtil.RandomMatrixTest

allTests = TestList [ TestLabel "Matrix.State Tests" testsMatrix
                    , TestLabel "RandomUtil.RandomMatrix Tests" testsRandomMatrix]
main = runTestTT allTests
