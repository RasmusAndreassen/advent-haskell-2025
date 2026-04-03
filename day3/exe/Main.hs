{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Arrow
import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Monoid (Any)

tmp' :: Char -> State (Char, Char) ()
tmp' d = do
  (i, j) <- get
  if j > i
    then do
      _1 .= j
      _2 .= d
    else do
      _2 .= max d j

tmp :: String -> Int
tmp s =
  let (d1, d2) = execState (mapM_ tmp' s) ('0', '0')
   in read [d1, d2]

data Bubbler a = B
  { _content :: [a],
    _capacity :: Int
  }
  deriving (Show)

emptyBubbler :: Int -> Bubbler a
emptyBubbler = B []

makeLenses 'B

full :: State (Bubbler a) Bool
full = do
  l <- use $ content . to length
  cap <- use capacity
  return $ l == cap

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

bubble :: (Ord a) => a -> State (Bubbler a) ()
bubble a = do
  f <- full
  if f
    then do
      c <- use content
      let (c', didContract) = contract c
      if didContract
        then
          content .= a : c'
        else do
          mh <- preuse $ content . _head
          whenJust mh $ \h ->
            when (a > h) $
              content . _head .= a
    else do
      content <|= a

contract :: (Ord a) => [a] -> ([a], Bool)
contract = foldr n ([], False)
  where
    n a ([], _) = ([a], False)
    n a1 (a2 : rest, False)
      | a1 > a2 = (a1 : rest, True)
      | otherwise = (a1 : a2 : rest, False)
    n a (as, True) = (a : as, True)

tmp2 :: String -> Int
tmp2 = read . bubbleLargest
  where
    bubbleLargest :: String -> String
    bubbleLargest s =
      let r = emptyBubbler 12 &~ mapM_ bubble s
       in r ^. (content . to reverse)

main :: IO ()
main = do
  interact $ lines >>> map tmp2 >>> sum >>> show
