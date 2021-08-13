{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set3 where

import MCPrelude
import Set4

instance Monad [] where
  return a = [a]
  bind xs fn = go xs []
   where
    go [] accum = accum
    go (x : xs) accum =
      let xs' = fn x
       in go xs (accum ++ xs')

data Card = Card Int String

instance Show Card where
  show (Card n s) = show n ++ s

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = liftM2 (,)

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 Card
