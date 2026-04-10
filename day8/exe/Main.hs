module Main (main) where

import Control.Arrow
import Data.Function
import Data.Map (Map, (!))
import Data.Map qualified as Map
import GHC.Arr (listArray)
import GHC.Real (infinity)
import Point

getLines :: IO [String]
getLines = lines <$> getContents

toFst :: (a -> b) -> a -> (b, a)
toFst f = f &&& id

toSnd :: (a -> b) -> a -> (a, b)
toSnd f = id &&& f

nn :: (a -> a -> d) -> [a] -> Map a (a, Rational)
nn d as =
  let arr = listArray $ toSnd (,infinity) <$> as
   in foldl insert pl as
  where
    insert n_1 a =
      n_1

main :: IO ()
main = do
  rows <- getLines

  let points :: [Point] = read <$> rows

  print $ length rows
