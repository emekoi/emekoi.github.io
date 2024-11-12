module Util
    ( bind2
    , zipWithM'
    ) where

import Control.Monad

zipWithM' :: Applicative m => [a] -> [b] -> (a -> b -> m c) -> m [c]
zipWithM' xs ys f = zipWithM f xs ys

bind2 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
bind2 x y k = x >>= ((y >>=) . k)
