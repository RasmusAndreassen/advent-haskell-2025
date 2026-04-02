module Main (main) where
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid (Sum (getSum))
import Control.Arrow
import Control.Monad
import Text.Read (readMaybe)
import Data.Maybe (maybeToList)

type Dial = WriterT (Sum Int) (State Int) 
data Direction = L | R

parseTurn :: String -> Maybe (Direction, Int)
parseTurn ('L':nu) = do
  n <- readMaybe nu
  return (L, n)
parseTurn ('R':nu) = do
  n <- readMaybe nu
  return (R, n)
parseTurn _ = Nothing

click :: Direction -> Dial ()
click dir = do
  let step = case dir of
            R -> (+1)
            L -> (subtract 1)
  modify $ step >>> (`mod` 100)
  pos <- get
  when (pos == 0) $
    tell 1

turn :: (Direction, Int) -> Dial ()
turn (d,n) = replicateM_ n (click d)

main :: IO ()
main = do
  interact $ lines
    >>> takeWhile (not . null)
    >>> map parseTurn 
    >>> concatMap maybeToList
    >>> mapM turn
    >>> execWriterT 
    >>> (`evalState` 50)
    >>> getSum
    >>> show
    >>> (++ "\n")
