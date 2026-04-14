module Vector where

import Control.Monad (guard)

data Vector = V Int Int
  deriving (Show)

instance Num Vector where
  (V x1 y1) + (V x2 y2) = V (x1 + x2) (y1 + y2)
  (V x1 y1) - (V x2 y2) = V (x1 - x2) (y1 - y2)
  (V x1 y1) * (V x2 y2) = V (x1 * x2) (y1 * y2)
  abs (V x y) = V (abs x) (abs y)
  signum (V x y) = V (signum x) (signum y)
  fromInteger i = let i' = fromInteger i in V i' i'

instance Read Vector where
  readsPrec i s = do
    (x :: Int, rest) <- readsPrec i s
    guard (head rest == ',')
    (y :: Int, rest') <- readsPrec i $ tail rest
    return (V x y, rest')
