module Data.Matrix.Lens (sub) where

import Control.Arrow
import Control.Lens
import Control.Lens qualified as Mat
import Data.Matrix (Matrix (ncols, nrows), getElem, setElem, submatrix)

elemAt :: (Int, Int) -> Lens' (Matrix a) a
elemAt p@(i, j) = lens (getElem i j) (flip (`setElem` p))

size :: Getter (Matrix a) (Int, Int)
size = to (nrows &&& ncols)

writeSubMatrix :: Int -> Int -> Matrix a -> Matrix a -> Matrix a
writeSubMatrix i0 j0 as m =
  let (imax, jmax) = as ^. size
      ijs = [(i, j) | i <- [1 .. imax], j <- [1 .. jmax]]
      adj = ((+ i0) *** (+ j0))
      transfer m p = m & elemAt (adj p) .~ (as ^. elemAt p)
   in foldl transfer m ijs

sub :: (Int, Int) -> (Int, Int) -> Lens' (Matrix a) (Matrix a)
sub (i, j) (i', j') = lens (submatrix i i' j j') (writeSubMatrix i j)
