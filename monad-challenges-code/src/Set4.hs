{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}

module Set4 where

import MCPrelude

class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a

sequence :: Monad m => [m a] -> m [a]
sequence = foldl (\mas ma -> bind mas (\as -> bind ma (\a -> return (a : as)))) (return [])

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 fn ma mb =
    bind ma $ \a -> bind mb $ \b -> return (fn a b)

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join mma = bind mma $ \ma -> bind ma return

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 fn ma mb mc = bind ma $ \a -> bind mb $ \b -> bind mc $ \c -> return $ fn a b c

ap :: Monad m => m (a -> b) -> m a -> m b
ap mab ma = bind mab $ \ab -> bind ma $ \a -> return $ ab a