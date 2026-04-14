module Util where

import Control.Arrow

foltr :: ([a] -> b -> b) -> b -> [a] -> b
foltr _ b0 [] = b0
foltr f b0 as@(_ : tail) =
  let b = foltr f b0 tail
      b' = f as b
   in b'

getLines :: IO [String]
getLines = lines <$> getContents

toFst :: (a -> b) -> a -> (b, a)
toFst f = f &&& id

toSnd :: (a -> b) -> a -> (a, b)
toSnd f = id &&& f

infixr 9 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

pick :: (a -> Bool) -> [a] -> Maybe (a, [a])
pick p (a : as)
  | p a = Just (a, as)
  | otherwise = second (a :) <$> pick p as
pick _ [] = Nothing
