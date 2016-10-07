module RecursionSchemes where

import FixedPoint

cata :: Functor f => (f a -> a) -> (Fix f -> a) -> Fix f -> a
cata psi f = psi . fmap f . out

cataRec :: Functor f => (f a -> a) -> Fix f -> a
cataRec psi = fix (cata psi)
