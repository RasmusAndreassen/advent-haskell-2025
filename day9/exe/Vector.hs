{-# LANGUAGE TemplateHaskell #-}

module Vector where

import Control.Lens (Traversal', both, makeLenses, traversal)
import Control.Monad (guard)
import Data.Bitraversable (Bitraversable)

data Vector = V
  { _x :: Int,
    _y :: Int
  }
  deriving (Show, Eq, Ord, Bounded)

makeLenses ''Vector

instance Num Vector where
  (V x1 y1) + (V x2 y2) = V (x1 + x2) (y1 + y2)
  (V x1 y1) - (V x2 y2) = V (x1 - x2) (y1 - y2)
  (V x1 y1) * (V x2 y2) = V (x1 * x2) (y1 * y2)
  abs (V x y) = V (abs x) (abs y)
  signum (V x y) = V (signum x) (signum y)
  fromInteger i = let i' = fromInteger i in V i' i'

coords :: Traversal' Vector (Int, Int)
coords f (V x y) = uncurry V <$> f (x, y)

instance Read Vector where
  readsPrec i s = do
    (x :: Int, rest) <- readsPrec i s
    guard (head rest == ',')
    (y :: Int, rest') <- readsPrec i $ tail rest
    return (V x y, rest')
