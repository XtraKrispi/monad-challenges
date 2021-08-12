{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Just a == Just b = a == b
    _ == _ = False

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_ : xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay a [] = Nothing
lookupMay a ((a', b) : xs)
    | a == a' = Just b
    | otherwise = lookupMay a xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a 0 = Nothing
divMay a b = Just $ a / b

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x : xs) = go x xs
  where
    go max [] = Just max
    go max (x : xs) = go (if max < x then x else max) xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x : xs) = go x xs
  where
    go min [] = Just min
    go min (x : xs) = go (if min > x then x else min) xs

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d key = case lookupMay key d of
    Nothing -> Nothing
    Just ns -> case (tailMay ns, headMay ns) of
        (Just is, Just h) -> case maximumMay is of
            Nothing -> Nothing
            Just n -> divMay (fromIntegral n) (fromIntegral h)
        _ -> Nothing

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain fn ma =
    case ma of
        Nothing -> Nothing
        Just a -> fn a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 d key =
    link (lookupMay key d) $
        \ns -> link (tailMay ns) $
            \is -> link (headMay ns) $
                \h -> link (maximumMay is) $
                    \n -> divMay (fromIntegral n) (fromIntegral h)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries d key1 key2 =
    link (lookupMay key1 d) $ \v1 -> link (lookupMay key2 d) $ \v2 -> mkMaybe $ v1 + v2

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink fn ma mb =
    link ma $ \a -> link mb $ \b -> mkMaybe $ fn a b

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 d key1 key2 = yLink (+) (lookupMay key1 d) (lookupMay key2 d)

mkMaybe :: a -> Maybe a
mkMaybe = Just

tailProd :: Num a => [a] -> Maybe a
tailProd [] = Nothing
tailProd [_] = mkMaybe 1
tailProd xs = transMaybe product $ tailMay xs

tailSum :: Num a => [a] -> Maybe a
tailSum [] = Nothing
tailSum [_] = mkMaybe 1
tailSum xs = transMaybe sum $ tailMay xs

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe _ Nothing = Nothing
transMaybe fn (Just a) = mkMaybe $ fn a

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax = transMaybe maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin = transMaybe minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just a) = a