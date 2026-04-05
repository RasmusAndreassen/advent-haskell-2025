{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Arrow (returnA)
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Monoid (Sum)

data Cell
  = Emitter
  | Space Bool
  | Splitter
  deriving (Eq)

makePrisms ''Cell

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

propagateCell :: Cell -> Cell -> Reader Bool Cell
propagateCell above (Space _) = do
  overflow <- ask
  return $ case above of
    Space b -> Space $ b || overflow
    _ -> Space overflow
propagateCell _ cell = return cell

cellOverflows :: Bool -> Cell -> Bool
cellOverflows True Splitter = True
cellOverflows _ _ = False

cellEmits :: Cell -> Bool
cellEmits (Space b) = b
cellEmits Emitter = True
cellEmits _ = False

infixr 9 <<

(<<) :: (c -> c') -> (a -> b -> c) -> a -> b -> c'
(<<) = (.) . (.)

n = proc c -> do
  rec (a, b) <- splitAt 2 -< b
  returnA -< a

propagateRow :: Row -> Row -> Row
propagateRow = (fst . foldr computeCell ([], False)) << zip

computeCell :: (Cell, Cell) -> ([Cell], Bool) -> ([Cell], Bool)
computeCell (above, cell) (hitherto, fromRight) =
  let incoming = cellEmits above
      cell' = runReader (propagateCell above cell) $ incoming || fromRight
      spill = cellOverflows incoming cell
      hitherto' =
        if spill
          then hitherto & (_head . _Space) ^~ True
          else hitherto
   in (cell' : hitherto', spill)

record :: (a -> b -> s) -> (a -> b) -> a -> Writer s b
record r f a = do
  let b = f a
  tell $ r a b
  return b

splits :: Cell -> Cell -> Int
splits above cell =
  if above == Space True && cell == Splitter
    then 1
    else 0

main :: IO ()
main = do
  ls <- lines <$> getContents
  let circuit = map fromString ls
      (executed, result) = runWriter $ scanlA1 (propagateRow count) circuit

  putStr $ unlines $ concatMap show <$> executed

  print $ length result
