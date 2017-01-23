import Test.HUnit
import Matrix.StateTest
import RandomUtil.RandomMatrixTest
import NN.NeuralNetworkTest

allTests = TestList [ TestLabel "Matrix.State Tests" testsMatrix
                    , TestLabel "RandomUtil.RandomMatrix Tests" testsRandomMatrix
                    , TestLabel "NN.NeuralNetwork Tests" testsNeuralNetwork]
main = runTestTT allTests
