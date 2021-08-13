{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Set1 where

import MCPrelude
import Set4

newtype Gen a = Gen {runGen :: Seed -> (a, Seed)}

evalGen :: Gen a -> Seed -> a
evalGen gen = fst . runGen gen

instance Monad Gen where
    return a = Gen $ \s -> (a, s)
    bind (Gen sa) fn = Gen $ \s ->
        let (a, s') = sa s
            Gen sb = fn a
         in sb s'

fiveRands :: [Integer]
fiveRands =
    let (i1, s) = rand (mkSeed 1)
        (i2, s') = rand s
        (i3, s'') = rand s'
        (i4, s''') = rand s''
        (i5, _) = rand s'''
     in [i1, i2, i3, i4, i5]

randGen :: Gen Integer
randGen = Gen rand

randLetter :: Gen Char
randLetter = Gen $ \s ->
    let (i, s') = rand s
     in (toLetter i, s')

randString3 :: String
randString3 = evalGen (liftM3 (\a b c -> [a, b, c]) randLetter randLetter randLetter) $ mkSeed 1

generalA :: (a -> b) -> Gen a -> Gen b
generalA fn (Gen ga) = Gen $ \s ->
    let (a, s') = ga s
     in (fn a, s')

randEven :: Gen Integer
randEven = generalA (* 2) randGen

randOdd :: Gen Integer
randOdd = generalA (+ 1) randEven

rand10 :: Gen Integer
rand10 = generalA (* 10) (Gen rand)

randPair :: Gen (Char, Integer)
randPair = bind randLetter $ \c -> bind randGen $ \i -> return (c, i)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = bind ga $ \a -> bind gb $ \b -> return (a, b)

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter randGen

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = liftM2 (,)
