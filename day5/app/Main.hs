module Main (main) where

import Control.Arrow

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p = second (dropWhile p) . break p

parseRange :: String -> (Int, Int)
parseRange = (read *** read) . break' (== '-')

between :: (Ord a) => a -> (a, a) -> Bool
between x (lo, hi) = lo <= x && x <= hi

main :: IO ()
main = do
  l <- lines <$> getContents
  -- let (rangesS, idsS) = break' null l
  --     ids :: [Int] = read <$> idsS
  --     ranges = parseRange <$> rangesS
  let rangesS = takeWhile (not . null) l
      ranges = parseRange <$> rangesS
      merged = mergeRanges ranges
      lengths = rangeLength <$> merged

  print $ sum lengths

-- print $ length $ filter (\x -> (x `between`) `any` ranges) ids

within :: (Ord a) => (a, a) -> (a, a) -> Bool
within (a1, a2) b = a1 `between` b && a2 `between` b

combination :: (Ord a) => (a, a) -> (a, a) -> Maybe (a, a)
combination a@(a1, a2) b@(b1, b2)
  | a `within` b = Just b
  | b `within` a = Just a
  | a1 `between` b = Just (b1, a2)
  | b1 `between` a = Just (a1, b2)
  | otherwise = Nothing

rangeLength :: (Int, Int) -> Int
rangeLength (lo, hi) = hi - lo + 1

mergeRanges :: (Ord a) => [(a, a)] -> [(a, a)]
mergeRanges = foldr mergeInRange []

mergeInRange :: (Ord a) => (a, a) -> [(a, a)] -> [(a, a)]
mergeInRange = (uncurry (:) .) . mergeInRange'

mergeInRange' :: (Ord a) => (a, a) -> [(a, a)] -> ((a, a), [(a, a)])
mergeInRange' c = foldr combine (c, [])
  where
    combine z (t, zs) =
      case combination t z of
        Just t' -> (t', zs)
        Nothing -> (t, z : zs)
