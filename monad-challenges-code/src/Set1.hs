{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands =
    let (i1, s) = rand (mkSeed 1)
        (i2, s') = rand s
        (i3, s'') = rand s'
        (i4, s''') = rand s''
        (i5, _) = rand s'''
     in [i1, i2, i3, i4, i5]

randLetter :: Gen Char
randLetter s =
    let (i, s') = rand s
     in (toLetter i, s')

randString3 :: String
randString3 =
    let (c1, s) = randLetter (mkSeed 1)
        (c2, s') = randLetter s
        (c3, s'') = randLetter s'
     in [c1, c2, c3]

generalA :: (a -> b) -> Gen a -> Gen b
generalA fn ga s =
    let (a, s') = ga s
     in (fn a, s')

randEven :: Gen Integer
randEven = generalA (* 2) rand

randOdd :: Gen Integer
randOdd = generalA (+ 1) randEven

rand10 :: Gen Integer
rand10 = generalA (* 10) rand

randPair :: Gen (Char, Integer)
randPair s =
    let (c, s') = randLetter s
     in generalA (c,) rand s'

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb s =
    let (a, s') = ga s
     in generalA (a,) gb s'

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB fn ga gb s =
    let (a, s') = ga s
     in generalA (fn a) gb s'

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

repRandom :: [Gen a] -> Gen [a]
repRandom xs s =
    foldl
        (\(xs', s') ga -> generalA (\x -> xs' ++ [x]) ga s')
        ([], s)
        xs

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga fn s =
    let (a, s') = ga s
     in fn a s'

mkGen :: a -> Gen a
mkGen = (,)