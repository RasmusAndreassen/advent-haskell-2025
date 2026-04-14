module Main (main) where

import Arith
import Control.Lens (both, to, view)
import Control.Monad
import Control.Monad.State (MonadState (put), State, evalState, gets, modify)
import Data.List (head, sortBy)
import Data.Maybe (fromJust, isNothing)
import Data.Monoid (Product (Product))
import Data.Ord (comparing)
import Point
import Util
import Prelude hiding (head, tail, (-))

distance :: Point -> Point -> Int
distance p q = size $ p - q

collect :: (Eq a) => (a, a) -> State [[a]] (a, a)
collect (a0, a1) = do
  m0 <- gets $ pick (a0 `elem`)
  m1 <- gets $ pick (a1 `elem`)
  case (m0, m1) of
    (Just (c0, others), Just (c1, _)) -> when (c0 /= c1) . put $ (c0 ++ c1) : filter (/= c1) others
    (Nothing, Just (c1, others)) -> put $ (a0 : c1) : others
    (Just (c0, others), Nothing) -> put $ (a1 : c0) : others
    (Nothing, Nothing) -> modify ([a0, a1] :)
  return (a0, a1)

recordLast :: (a, a) -> State [[a]] (Maybe (a, a))
recordLast link = do
  l <- gets length
  return $
    if l == 1
      then Just link
      else Nothing

allConnections :: (Eq a) => (a -> a -> Int) -> [a] -> [(a, a)]
allConnections d as = map fst . sortBy (comparing snd) $ do
  a1 <- as
  a2 <- takeWhile (/= a1) as
  return ((a1, a2), d a1 a2)

main :: IO ()
main = do
  points :: [Point] <- map read <$> getLines

  let connections = allConnections distance points
      clusterS = mapM (collect >=> recordLast) connections
      results = evalState clusterS $ pure <$> points
      lastLink = fromJust . head . dropWhile isNothing $ results

  print $ view (both . x . to Product) lastLink
