{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Monoid (Sum (Sum, getSum))
import GHC.Stack (HasCallStack)

data Cell
  = Emitter
  | Space Int
  | Splitter
  deriving (Eq)

makePrisms ''Cell

type Row = [Cell]

instance Show Cell where
  show Emitter = "S"
  show (Space n) = if n > 0 then "|" else "."
  show Splitter = "^"

fromString :: String -> Row
fromString = map conv
  where
    conv '.' = Space 0
    conv '^' = Splitter
    conv 'S' = Emitter
    conv _ = undefined

scanlM :: (Monad f) => (b -> a -> f b) -> b -> [a] -> f [b]
scanlM (<:>) nl (a : as) = do
  b <- nl <:> a
  bs <- scanlM (<:>) b as
  return $ b : bs
scanlM _ _ [] = return []

scanlM1 :: (HasCallStack, Monad f) => (a -> a -> f a) -> [a] -> f [a]
scanlM1 (<:>) (a : as) = scanlM (<:>) a as
scanlM1 _ [] = undefined

cellOverflows :: Int -> Cell -> Int
cellOverflows n Splitter = n
cellOverflows _ _ = 0

cellEmits :: Cell -> Int
cellEmits (Space b) = b
cellEmits Emitter = 1
cellEmits _ = 0

infixr 9 <<

(<<) :: (c -> c') -> (a -> b -> c) -> a -> b -> c'
(<<) = (.) . (.)

showRow :: Row -> String
showRow = concatMap show

propagateRow :: Row -> Row -> Row
propagateRow = inner << zip
  where
    inner [] = []
    inner ps = evalState (inner' ps) 0
    inner' :: [(Cell, Cell)] -> State Int Row
    inner' [] = do
      put 0
      return []
    inner' ((above, cell) : ps) = do
      fromRight <- get
      let incoming = cellEmits above
          spill = cellOverflows incoming cell
      put spill
      cells' <- inner' ps
      fromLeft <- get
      let cell' = set _Space (incoming + fromRight + fromLeft) cell
      put spill
      return $ cell' : cells'

pathCounts :: Traversal' Row Int
pathCounts = traversal $ \f r ->
  sequenceA $ flip map r $ \case
    Space n -> Space <$> f n
    other -> pure other

main :: IO ()
main = do
  ls <- lines <$> getContents
  let circuit = map fromString ls
      executed = scanl1 propagateRow circuit
      paths = getSum $ view (_last . pathCounts . to Sum) executed

  putStr $ unlines . map showRow $ executed
  putStrLn $ "total " ++ show paths ++ " paths"
