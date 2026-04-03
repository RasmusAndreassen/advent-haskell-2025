module Main (main) where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Arrow (Arrow (first, second))
import Control.Lens (ala, unsnoc)

type Binop a = a -> a -> a

operator :: Char -> Binop Integer
operator '+' = (+)
operator '*' = (*)
operator _ = undefined

transpose :: [[a]] -> [[a]]
transpose = ala ZipList traverse

columnize :: String -> [(Binop Integer, Int)]
columnize "" = []
columnize (op : rest) =
  let (space, rest') = span (== ' ') rest
      width = if null rest' then length space + 1 else length space
   in (operator op, width) : columnize rest'

split :: [Int] -> [a] -> [[a]]
split (n : ns) as = let (h, t) = splitAt n as in h : split ns (drop 1 t)
split _ _ = []

columns :: String -> ([[String]], [Binop Integer])
columns input =
  let Just (operandStrings, operatorString) = unsnoc . lines $ input
      (operators, widths) = unzip . columnize $ operatorString
      operandRows = split widths <$> operandStrings
      operandColumns = transpose operandRows
   in (operandColumns, operators)

computation :: String -> [Integer]
computation input =
  let (operandColumns, operators) = columns input
      operatorz = ZipList operators
      operands = map read . transpose <$> operandColumns
      operandz = ZipList <$> transpose operands
      results = getZipList $ foldA operatorz operandz
   in results

main :: IO ()
main = do
  input <- getContents

  print $ sum $ computation input

liftP2 :: (Applicative f) => f (a -> b -> c) -> f a -> f b -> f c
liftP2 nf na = (nf <*> na <*>)

foldA :: (Foldable t, Applicative f) => f (a -> a -> a) -> t (f a) -> f a
foldA mf = foldl1 $ liftP2 mf
