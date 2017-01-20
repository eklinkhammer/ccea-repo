module RandomUtil.RandomMatrixTest
  (
    testsRandomMatrix
  ) where

import Test.HUnit

import Numeric.LinearAlgebra.HMatrix hiding (corr)
import System.Random

import RandomUtil.RandomMatrix
import Matrix.State

mat = reshape 3 $ fromList $ take 9 [1..] :: Matrix Double

data Two = Two

instance RandomGen Two where
  next g  = (2, g)
  split g = (g, g)
  
testsRandomMatrix = TestList [ TestLabel "selectRandomMatrixElement" testsSelectRandomMatrixElement,
                               TestLabel "randomMatrixIndex" testsRandomMatrixIndex
                             , TestLabel "randomizeRandomMatrixElement" testsRandomizeRandomMatrixElement]


testsSelectRandomMatrixElement = TestList [TestLabel "test1" testSelects]
testsRandomMatrixIndex = TestList [TestLabel "test1" testInBounds]
testsRandomizeRandomMatrixElement = TestList [TestLabel "test1" testMutates]

-- testsSelectRandomMatrixElement
testSelects = TestCase (assertEqual "selectRandomMatrixElement - Select"
                         5
                         (selectRandomMatrixElement Two mat))

-- testsRandomMatrixIndex
testInBounds = TestCase (do gen <- getStdGen
                            let listI  = getListRandomIndex gen 100 mat
                            let result = checkAllTrue listI (isValid mat)
                            assertEqual "randomMatrixIndex - Indices in bounds" True result)

-- testsRandomizeMatrixElement
testMutates = TestCase (do
                           let rMat = randomizeRandomMatrixElement Two Two mat
                           let val  = rMat ! 1 ! 1
                           let rVal = fst $ randomR ((-10), 10) Two :: Double
                           assertEqual "randomizeRandomMatrixElement" rVal val)
              
-- testsRandomMatrixIndex helper function
isValid :: Matrix a -> MatrixIndex -> Bool
isValid m (i,j) = i >= 0 && j >= 0 && i < rows m && j < cols m

getListRandomIndex :: (Element a) => StdGen -> Int -> Matrix a -> [MatrixIndex]
getListRandomIndex _ 0 _ = []
getListRandomIndex g n m = rIndex : getListRandomIndex newG (n-1) m
  where
    rIndex = randomMatrixIndex g m
    newG   = snd $ next g

checkAllTrue :: [a] -> (a -> Bool) -> Bool
checkAllTrue xs f = foldr (&&) True $ map f xs
