module Main (main) where
import Text.Read (readMaybe)
import Data.Maybe (maybeToList)
import Control.Arrow
import Data.List (sort)
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad (forM)

breakOn :: (a -> Bool) -> [a] -> ([a], [a])
breakOn p = break p >>> (second $ drop 1)

split :: (a -> Bool) -> [a] -> [[a]]
split p as = case breakOn p as of
              ([], []) -> []
              (before, after) -> before : split p after

parseRange :: String -> Maybe (Integer, Integer)
parseRange s = do
  let (before, after) = breakOn (== '-') s
  stop <- readMaybe after
  start <- readMaybe before
  return (start, stop)

nDigits :: Integral i => i -> i
nDigits = ceiling . (logBase 10 :: Float -> Float) . fromIntegral . (+ 1)

dRepeat :: Integral i => i -> [i]
dRepeat n = let digits = nDigits n in iterate ((+n).(*10^digits)) n

rangeDups :: Integral i => (i,i) -> [i]
rangeDups (start, end) = do
  n <- [1..end]
  takeWhile (<= end) . dropWhile (< start) . drop 1 $ dRepeat n

removeDups :: Ord a => [a] -> [a]
removeDups = map NonEmpty.head . NonEmpty.group . sort
  
rangeBad :: Integral i => (i,i) -> [i]
rangeBad = removeDups . rangeDups

main :: IO ()
main = do
  line <- getLine
  let rangeStrings = split (== ',') line
  
  dups <-
    forM rangeStrings $ \string -> do
      putStrLn $ "checking " ++ string
      case parseRange string of
        Just range -> do
          let dups = rangeBad range
          putStrLn "done"
          print $ sum dups
          return dups
        Nothing    -> do
          putStrLn "invalid range"
          return []

  putStrLn "summing"
  print $ sum $ concat dups

