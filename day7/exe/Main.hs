module Main (main) where

import Control.Monad (when)
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Monoid (Sum (getSum))

data Cell
  = Emitter
  | Space Bool
  | Splitter
  deriving (Eq)

type Row = [Cell]

instance Show Cell where
  show Emitter = "S"
  show (Space b) = if b then "|" else "."
  show Splitter = "^"

fromString :: String -> Row
fromString = map conv
  where
    conv '.' = Space False
    conv '|' = Space True
    conv '^' = Splitter
    conv 'S' = Emitter
    conv _ = undefined

foldrA :: (Applicative f) => (a -> f b -> f b) -> b -> [a] -> f b
foldrA op b = foldr op (pure b)

scanlA1 :: (Applicative f) => (f a -> a -> f a) -> [a] -> f [a]
scanlA1 op (a : as) = sequenceA . scanl op (pure a) $ as
scanlA1 _ [] = undefined

propagateCell :: Cell -> Cell -> Bool -> Cell
propagateCell above (Space _) overflow = case above of
  Space b -> Space $ b || overflow
  _ -> Space overflow
propagateCell _ cell _ = cell

cellOverflows :: Bool -> Cell -> Bool
cellOverflows True Splitter = True
cellOverflows _ _ = False

cellEmits :: Cell -> Bool
cellEmits (Space b) = b
cellEmits Emitter = True
cellEmits _ = False

propagateRow :: Row -> Row -> Row
propagateRow prev curr = fst . foldr computeCell ([], False) $ zip prev curr
  where
    computeCell (above, cell) (hitherto, fromRight) =
      let incoming = cellEmits above
          cell' = propagateCell above cell (incoming || fromRight)
          spill = cellOverflows incoming cell
          hitherto' =
            if spill
              then case hitherto of
                (Space False) : thitherto -> Space True : thitherto
                other -> other
              else hitherto
       in (cell' : hitherto', spill)

record :: ((a, b) -> s) -> (a -> b) -> a -> Writer s b
record r f a = do
  let b = f a
  tell $ r (a, b)
  return b

count :: Cell -> Cell -> Writer String ()
count above cell = do
  when (cell == Splitter) $
    tell $
      show above

main :: IO ()
main = do
  ls <- lines <$> getContents
  let circuit = map fromString ls
      (executed, result) = runWriter $ scanlA1 (propagateRow count) circuit

  putStr $ unlines $ concatMap show <$> executed

  print $ length result
