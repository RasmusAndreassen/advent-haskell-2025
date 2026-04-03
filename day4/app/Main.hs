module Main (main) where

import Control.Lens (ala)
import Data.Matrix
import Data.Maybe (maybeToList)
import Data.Monoid (Sum (Sum))

data Slot
  = Empty
  | Filled
  deriving (Eq)

instance Show Slot where
  show Empty = "."
  show Filled = "@"

fromChar :: Char -> Slot
fromChar '.' = Empty
fromChar _ = Filled

input :: IO (Matrix Slot)
input = do
  inputStream <- lines <$> getContents
  let charMatrix = fromLists inputStream
  return $ mapPos (const fromChar) charMatrix

count :: (Traversable t) => (a -> Bool) -> t a -> Int
count p = ala Sum foldMap . fmap (\x -> if p x then 1 else 0)

accessible :: Matrix Slot -> Matrix Bool
accessible m = mapPos f m
  where
    f (x, y) s =
      let isFilled = s == Filled
          surroundingIdx =
            [ (xi, yi)
            | xi <- [x - 1 .. x + 1],
              yi <- [y - 1 .. y + 1],
              (xi, yi) /= (x, y)
            ]
          surrounding = uncurry safeGet <$> surroundingIdx <*> pure m
          surrounding' = maybeToList =<< surrounding
       in isFilled && count (== Filled) surrounding' < 4

removeAccessible :: Matrix Slot -> Matrix Slot
removeAccessible m = elementwise remove m (accessible m)
  where
    remove Empty _ = Empty
    remove Filled a = if a then Empty else Filled

main :: IO ()
main = do
  m <- input

  let stages = iterate removeAccessible m
      removable = count id . accessible <$> stages
      total = sum $ takeWhile (> 0) removable

  print total
