{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Monoid (Sum (Sum))
import GHC.Stack (HasCallStack)

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

scanlM :: (Monad f) => (b -> a -> f b) -> b -> [a] -> f [b]
scanlM (<:>) nl (a : as) = do
  b <- nl <:> a
  bs <- scanlM (<:>) b as
  return $ b : bs
scanlM _ _ [] = return []

scanlM1 :: (HasCallStack, Monad f) => (a -> a -> f a) -> [a] -> f [a]
scanlM1 (<:>) (a : as) = scanlM (<:>) a as
scanlM1 _ [] = undefined

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

showRow :: Row -> String
showRow = concatMap show

propagateRow :: Row -> Row -> Writer (Sum Int) Row
propagateRow = inner << zip
  where
    inner [] = return []
    inner ps = evalStateT (inner' ps) False
    inner' :: [(Cell, Cell)] -> StateT Bool (Writer (Sum Int)) Row
    inner' [] = do
      put False
      return []
    inner' ((above, cell) : ps) = do
      fromRight <- get
      let incoming = cellEmits above
          spill = cellOverflows incoming cell
      put spill
      cells' <- inner' ps
      fromLeft <- get
      let cell' = set _Space (incoming || fromRight || fromLeft) cell
      put spill
      when spill $
        lift $
          tell 1
      return $ cell' : cells'

main :: IO ()
main = do
  ls <- lines <$> getContents
  let circuit = map fromString ls
      executed = scanlM1 propagateRow circuit
      (outputMap, Sum count) = runWriter executed

  putStr $ unlines . map showRow $ outputMap
  putStrLn $ "split " ++ show count ++ " times"
