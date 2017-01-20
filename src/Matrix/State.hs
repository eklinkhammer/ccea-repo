module Matrix.State
  (
    replaceMatrixElement
  , replaceVector
  , replaceElement
  , MatrixIndex (..)
  , VectorIndex (..)
  , MatrixMutation(..)
  , VectorMutation (..)
  ) where

import Numeric.LinearAlgebra.HMatrix hiding (corr)

type MatrixIndex      = (MatrixRowIndex, VectorIndex)
type VectorIndex      = Int
type MatrixRowIndex   = Int
type MatrixMutation a = Matrix a -> Matrix a
type VectorMutation a = Vector a -> Vector a

replaceMatrixElement :: (Element a) => MatrixIndex -> a -> MatrixMutation a
replaceMatrixElement (i,j) e m = replaceVector i v' m
  where
    v  = m ! i
    v' = replaceElement j e v

replaceVector :: (Element a) => MatrixRowIndex -> Vector a -> MatrixMutation a
replaceVector i v m = if not $ inBoundsMatrixRow i m
                      then m
                      else fromRows $ rowsAbove ++ v : rowsBelow
  where
    rowsAbove = toRows $ takeRows i       m
    rowsBelow = toRows $ dropRows (i + 1) m

replaceElement :: (Element a) => VectorIndex -> a -> VectorMutation a
replaceElement i e v = if not $ inBounds i v
                       then v
                       else fromList $ elemsBefore ++ e : elemsAfter
  where
    elemsBefore = take  i      $ lV
    elemsAfter  = drop (i + 1) $ lV
    lV         = toList v
    

inBounds :: (Element a) => VectorIndex -> Vector a -> Bool
inBounds i v = (i >= 0) && (i < (length $ toList v))

inBoundsMatrixRow :: (Element a) => MatrixRowIndex -> Matrix a -> Bool
inBoundsMatrixRow i m = (i >= 0) && (i < (rows m))
