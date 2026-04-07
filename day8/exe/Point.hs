module Point (Point (P, _x, _y, _z), distance) where

import Arith
import Prelude hiding ((+), (-))

data Point = P
  { _x :: Int,
    _y :: Int,
    _z :: Int
  }
  deriving (Eq, Show)

distance :: Point -> Point -> Int
distance (P x1 y1 z1) (P x2 y2 z2) =
  let dx = x2 - x1
      dy = y2 - y1
      dz = z2 - z1
   in dx ^ 2 + dy ^ 2 + dz ^ 2

instance Read Point where
  readsPrec i s = do
    (x :: Int, rest) <- readsPrec i s
    (y :: Int, rest') <- readsPrec i $ drop 1 rest
    (z :: Int, rest'') <- readsPrec i $ drop 1 rest'
    return (P x y z, rest'')

instance Neg Point Point where
  negate (P x y z) = P (-x) (-y) (-z)

instance Add Point Point Point where
  (P x1 y1 z1) + (P x2 y2 z2) = P (x1 + x2) (y1 + y2) (z1 + z2)

instance Add Point Int Point where
  (P x y z) + c = P (x + c) (y + c) (z + c)

instance Sub Point Point Point where
  (P x1 y1 z1) - (P x2 y2 z2) = P (x1 - x2) (y1 - y2) (z1 - z2)

instance Sub Point Int Point where
  (P x y z) - c = P (x - c) (y - c) (z - c)
