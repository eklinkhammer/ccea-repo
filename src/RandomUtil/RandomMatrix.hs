{-# LANGUAGE FlexibleContexts #-}

module RandomUtil.RandomMatrix
  (
    selectRandomMatrixElement
  , randomMatrixIndex
  , randomizeRandomMatrixElement
  , randomizeNRandomMatrixElements
  ) where

import System.Random
import Matrix.State
import Numeric.LinearAlgebra.HMatrix hiding (corr)


selectRandomMatrixElement ::
  ( Element a
  , Indexable (Vector a) a
  , RandomGen g)
  => g -> Matrix a -> a
selectRandomMatrixElement g m = let (i,j) = randomMatrixIndex g m
                                in m ! i ! j

randomMatrixIndex :: (Element a, RandomGen g) => g -> Matrix a -> MatrixIndex
randomMatrixIndex g m = let (i, newG) = randomR (0, (rows m) - 1) g
                        in (i, fst (randomR (0, (cols m) - 1) g))

randomizeRandomMatrixElement :: ( Num a , Element a, Random a, Indexable (Vector a) a, RandomGen g) => g -> g -> MatrixMutation a
randomizeRandomMatrixElement indexGen elementGen m = let i = randomMatrixIndex indexGen m
                                                         e = fst $ randomR ((-10), 10) elementGen
                                                     in replaceMatrixElement i e m 


randomizeNRandomMatrixElements ::
  (Num a, Element a, Random a, Indexable (Vector a) a, RandomGen g) =>
  Int -> g -> g -> Matrix a -> (Matrix a, (g,g))
randomizeNRandomMatrixElements 0 indexGen elementGen m = (m, (indexGen, elementGen))
randomizeNRandomMatrixElements n indexGen elementGen m =
  let mutated            = randomizeRandomMatrixElement indexGen elementGen m
      (_, newIndexGen)   = next indexGen
      (_, newElementGen) = next elementGen
  in randomizeNRandomMatrixElements (n - 1) newIndexGen newElementGen mutated
