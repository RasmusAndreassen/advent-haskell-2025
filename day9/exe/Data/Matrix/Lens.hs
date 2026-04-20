module Data.Matrix.Lens
  ( sub,
    size,
    asRows,
    asCols,
    elemAt,
    eachPos,
    eachElem,
    eachRow,
    eachCol,
    row,
    col,
    rows,
    cols,
    removingRow,
    removingCol,
    removingRows,
    removingCols,
  )
where

import Control.Arrow
import Control.Lens
import Control.Lens qualified as Mat
import Control.Monad (guard)
import Data.Foldable (Foldable (fold, foldr', toList))
import Data.Matrix (Matrix (ncols, nrows), fromList, fromLists, getCol, getElem, getRow, mapCol, mapPos, mapRow, matrix, setElem, submatrix, toLists, transpose)
import Data.Monoid (Endo (Endo))
import Data.Vector qualified as Vec
import Util

elemAt :: (Int, Int) -> Lens' (Matrix a) a
elemAt p@(i, j) = lens (getElem i j) (flip (`setElem` p))

eachElem :: Traversal (Matrix a) (Matrix b) a b
eachElem = traverse

flattened :: Lens (Matrix a) (Matrix b) [a] [b]
flattened f xa =
  let (n, m) = xa ^. size
   in fromList n m <$> (f . toList $ xa)

eachPos :: IndexedTraversal (Int, Int) (Matrix a) (Matrix b) a b
eachPos = itraverse . indexed

eachRow :: Traversal (Matrix a) (Matrix b) [a] [b]
eachRow = asRows . each

eachCol :: Traversal (Matrix a) (Matrix b) [a] [b]
eachCol = asCols . each

asRows :: Iso (Matrix a) (Matrix b) [[a]] [[b]]
asRows = iso toLists fromLists

asCols :: Iso (Matrix a) (Matrix b) [[a]] [[b]]
asCols = transposed . asRows

transposed :: Iso (Matrix a) (Matrix b) (Matrix a) (Matrix b)
transposed = iso transpose transpose

setRow :: (Int -> a) -> Int -> Matrix a -> Matrix a
setRow as = mapRow (const . as)

setCol :: (Int -> a) -> Int -> Matrix a -> Matrix a
setCol as = mapCol (const . as)

setRowTo :: [a] -> Int -> Matrix a -> Matrix a
setRowTo as = setRow (\n -> as !! (n - 1))

setColTo :: [a] -> Int -> Matrix a -> Matrix a
setColTo as = setCol (\n -> as !! (n - 1))

row :: Int -> Lens' (Matrix a) [a]
row i = lens (toList . getRow i) (writeRow i)
  where
    writeRow i m r = setRowTo r i m

rows :: [Int] -> Lens' (Matrix a) (Matrix a)
rows is = lens (getter is) (setter is)
  where
    getter is x =
      let rs = (toList .: getRow) <$> is ?? x
       in fromLists rs
    setter is x rm =
      let rs = rm ^.. eachRow
          settings = zipWith setRowTo rs is
       in ala Endo foldMap settings x

col :: Int -> Lens' (Matrix a) [a]
col j = lens (toList . getCol j) (writeCol j)
  where
    writeCol j m c = setColTo c j m

cols :: [Int] -> Lens' (Matrix a) (Matrix a)
cols js = lens (getter js) (setter js)
  where
    getter js x =
      let cs = (toList .: getCol) <$> js ?? x
       in transpose $ fromLists cs
    setter js x cm =
      let cs = cm ^.. eachCol
          settings = zipWith setColTo cs js
       in ala Endo foldMap settings x

size :: Getter (Matrix a) (Int, Int)
size = to (nrows &&& ncols)

writeSubMatrix :: Int -> Int -> Matrix a -> Matrix a -> Matrix a
writeSubMatrix i0 j0 m as =
  let (imax, jmax) = as ^. size
      adj = ((+ (i0 - 1)) *** (+ (j0 - 1)))
      transfer p m a = m & elemAt (adj p) .~ a
   in ifoldl transfer m as

removeRow :: Int -> Matrix a -> Matrix a
removeRow r x =
  let (n, m) = x ^. size
      n' = n - 1
      adjusted p@(i, j)
        | i < r = p
        | otherwise = first (+ 1) p
      procure p = x ^. elemAt (adjusted p)
   in matrix n' m procure

removingRow :: Int -> Lens' (Matrix a) (Matrix a)
removingRow j = lens (removeRow j) (writeAroundRow j)
  where
    writeAroundRow r x s =
      let procure p@(i, j)
            | i < r = const $ s ^. elemAt p
            | i > r = const $ s ^. elemAt (i - 1, j)
            | otherwise = id
       in imap procure x

removeCol :: Int -> Matrix a -> Matrix a
removeCol c x =
  let (n, m) = x ^. size
      m' = m - 1
      adjusted p@(i, j)
        | j < c = p
        | otherwise = second (+ 1) p
      procure p = x ^. elemAt (adjusted p)
   in matrix n m' procure

removingCol :: Int -> Lens' (Matrix a) (Matrix a)
removingCol c = lens (removeCol c) (writeAroundCol c)
  where
    writeAroundCol c x s =
      let procure p@(i, j)
            | j < c = const $ s ^. elemAt p
            | j > c = const $ s ^. elemAt (i, j - 1)
            | otherwise = id
       in imap procure x

removingCols :: [Int] -> Lens' (Matrix a) (Matrix a)
removingCols (j : js) =
  let js' = js & traverse . filtered (> j) -~ 1
   in removingCol j . removingCols js'
removingCols [] = id

removingRows :: [Int] -> Lens' (Matrix a) (Matrix a)
removingRows (i : is) =
  let is' = is & traverse . filtered (> i) -~ 1
   in removingRow i . removingRows is'
removingRows [] = id

droppingCols :: Int -> Lens' (Matrix a) (Matrix a)
droppingCols m = removingCols [1 .. m]

droppingRows :: Int -> Lens' (Matrix a) (Matrix a)
droppingRows n = removingRows [1 .. n]

instance FunctorWithIndex (Int, Int) Matrix

instance FoldableWithIndex (Int, Int) Matrix

instance TraversableWithIndex (Int, Int) Matrix where
  itraverse f x =
    let x' = mapPos f x
     in sequenceA x'

sub :: (Int, Int) -> (Int, Int) -> Lens' (Matrix a) (Matrix a)
sub (i, j) (i', j') = lens (submatrix i i' j j') (writeSubMatrix i j)
