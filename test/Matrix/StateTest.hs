module Matrix.StateTest
  (
    testsMatrix
  ) where

import Test.HUnit

import Numeric.LinearAlgebra.HMatrix hiding (corr)
import Matrix.State

mat = reshape 3 $ fromList $ take 9 [1..] :: Matrix Double
vec = fromList $ take 3 [10..] :: Vector Double

test1RE = TestCase (assertEqual "replaceElement - first position"
                    (fromList [1,11,12])
                    (replaceElement 0 1 vec))

test2RE = TestCase (assertEqual "replaceElement - last position"
                    (fromList [10,11,1])
                    (replaceElement 2 1 vec))

test3RE = TestCase (assertEqual "replaceElement - in vector"
                    (fromList [10,2,12])
                    (replaceElement 1 2 vec))

test4RE = TestCase (assertEqual "replaceElement - out of bounds low"
                    (fromList [10,11,12])
                    (replaceElement (-1) 2 vec))
          
test5RE = TestCase (assertEqual "replaceElement - out of bounds high"
                    (fromList [10,11,12])
                    (replaceElement 4 2 vec))

testsReplaceElement = TestList [  TestLabel "test1" test1RE
                                , TestLabel "test2" test2RE
                                , TestLabel "test3" test3RE
                                , TestLabel "test4" test4RE
                                , TestLabel "test5" test5RE
                                ]

test1RV = TestCase (assertEqual "replaceVector - first row"
                    (reshape 3 $ fromList [10,11,12,4,5,6,7,8,9])
                    (replaceVector 0 vec mat))

test2RV = TestCase (assertEqual "replaceVector - inner row"
                    (reshape 3 $ fromList [1,2,3,10,11,12,7,8,9])
                    (replaceVector 1 vec mat))

test3RV = TestCase (assertEqual "replaceVector - last row"
                    (reshape 3 $ fromList [1,2,3,4,5,6,10,11,12])
                    (replaceVector 2 vec mat))

test4RV = TestCase (assertEqual "replaceVector - index out of bounds - low"
                    mat
                    (replaceVector (-1) vec mat))

test5RV = TestCase (assertEqual "replaceVector - index out of bounds - high"
                    mat
                    (replaceVector 10 vec mat))
          
testsReplaceVector = TestList [ TestLabel "test1" test1RV
                              , TestLabel "test2" test2RV
                              , TestLabel "test3" test3RV
                              , TestLabel "test4" test4RV
                              , TestLabel "test5" test5RV
                              ]

test1RME = TestCase (assertEqual "replaceMatrixElement - first row"
                     (reshape 3 $ fromList [0,2,3,4,5,6,7,8,9])
                     (replaceMatrixElement (0,0) 0 mat))

test2RME = TestCase (assertEqual "replaceMatrixElement - middle row"
                     (reshape 3 $ fromList [1,2,3,0,5,6,7,8,9])
                     (replaceMatrixElement (1,0) 0 mat))

test3RME = TestCase (assertEqual "replaceMatrixElement - end first row"
                     (reshape 3 $ fromList [1,2,0,4,5,6,7,8,9])
                     (replaceMatrixElement (0,2) 0 mat))

test4RME = TestCase (assertEqual "replaceMatrixElement - last row"
                     (reshape 3 $ fromList [1,2,3,4,5,6,7,8,0])
                     (replaceMatrixElement (2,2) 0 mat))

test5RME = TestCase (assertEqual "replaceMatrixElement - out of bounds"
                     mat
                     (replaceMatrixElement (0,4) 0 mat))

test6RME = TestCase (assertEqual "replaceMatrixElement - out of bounds"
                     mat
                     (replaceMatrixElement ((-1),0) 0 mat))


testsReplaceMatrixElement = TestList [ TestLabel "test1" test1RME
                                     , TestLabel "test2" test2RME
                                     , TestLabel "test3" test3RME
                                     , TestLabel "test4" test4RME
                                     , TestLabel "test5" test5RME
                                     , TestLabel "test6" test6RME]

testsMatrix = TestList [ TestLabel "replaceElementTests" testsReplaceElement
                       , TestLabel "replaceVectorTests" testsReplaceVector
                       , TestLabel "replaceMatrixElementTests" testsReplaceMatrixElement]
