module Point (Point, distance) where

data Point = P Int Int Int deriving (Eq, Show, Ord)

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
