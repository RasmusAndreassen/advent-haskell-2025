module Main (main) where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Lens (Rewrapping, Wrapped (Unwrapped), ala, unsnoc)
import Data.Maybe (fromJust)

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

columns :: String -> ([[Integer]], [Binop Integer])
columns input =
  let (operandStrings, operatorString) = fromJust . unsnoc . lines $ input
      (operators, widths) = unzip . columnize $ operatorString
      operandRows = split widths <$> operandStrings
      operandColumns = map read . transpose <$> transpose operandRows
   in (operandColumns, operators)

computation :: String -> [Integer]
computation input =
  let (operandColumns, operators) = columns input
      results = getZipList $ foldl1 <$> ZipList operators <*> ZipList operandColumns
   in results

main :: IO ()
main = do
  input <- getContents

  print $ sum $ computation input
