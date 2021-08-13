{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set3 where

import MCPrelude

data Card = Card Int String

instance Show Card where
  show (Card n s) = show n ++ s

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs fn as bs = fn `map` as `combStep` bs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 fn as bs cs = fn `map` as `combStep` bs `combStep` cs

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 fn as bs cs ds = fn `map` as `combStep` bs `combStep` cs `combStep` ds

allPairs :: [a] -> [b] -> [(a, b)]
allPairs = allCombs (,)

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

combStep :: [a -> b] -> [a] -> [b]
combStep fns as = go fns as
 where
  -- Traverse down the left list first
  go [] _ = []
  go (x : xs') [] = go xs' as
  go a@(x : xs') (y : ys') = x y : go a ys'