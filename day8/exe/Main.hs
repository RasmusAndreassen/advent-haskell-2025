module Main (main) where

import Arith
import Control.Lens
import Data.Function (on)
import Data.List (head, mapAccumL, sort, sortBy)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import GHC.Base (maxInt)
import Point
import Util
import Prelude hiding (head, tail, (-))

distance :: Point -> Point -> Int
distance p q = size $ p - q

slink :: (Point -> Point -> Int) -> [Point] -> [(Point, Point, Int)]
slink d ps =
  let hmapping :: Map Int (Int, Int) = foldl insert Map.empty (Map.keys parr)
   in map (\(i, (π, λ)) -> (p i, p π, λ)) . Map.toList $ hmapping
  where
    p = (parr !)
    parr :: Map Int Point = Map.fromAscList $ zip [1 ..] ps
    d' = d `on` p
    insert :: Map Int (Int, Int) -> Int -> Map Int (Int, Int)
    insert pacc n =
      let (_, ps') = Map.mapAccumWithKey (update n) Map.empty pacc
          ps'' = inc n <$> ps'
       in Map.insert n (n, maxInt) ps''
    update :: Int -> Map Int Int -> Int -> (Int, Int) -> (Map Int Int, (Int, Int))
    update n mm i l@(π, λ)
      | m < λ =
          let mm' = Map.adjust (min λ) π mm
           in (mm', (n, m))
      | otherwise =
          let mm' = Map.adjust (min m) π mm
           in (mm', l)
      where
        m = d' n i
    inc :: Int -> (Int, Int) -> (Int, Int)
    inc n l@(π, λ)
      | d' n π < λ = (n, λ)
      | otherwise = l

collect :: (Eq a) => [[a]] -> [(a, a)] -> [[a]]
collect = foldl insert
  where
    insert :: (Eq a) => [[a]] -> (a, a) -> [[a]]
    insert clusters (a0, a1) =
      case pick (a0 `elem`) clusters of
        Just (c1, clusters') ->
          case pick (a1 `elem`) clusters' of
            Just (c2, clusters'') ->
              if c1 == c2
                then clusters
                else (c1 ++ c2) : clusters''
            Nothing -> (a1 : c1) : clusters'
        Nothing -> [a0, a1] : clusters

main :: IO ()
main = do
  points :: [Point] <- map read <$> getLines

  let connections = slink distance points
      clusters =
        collect (pure <$> points)
          . take 1000
          . map (\(p, q, _) -> (p, q))
          . filter (view $ _3 . to (< maxInt))
          . sortBy (comparing $ view _3)
          $ connections
      lengths = Set.fromList . map length $ clusters

  print $ product . take 3 . Set.toDescList $ lengths
