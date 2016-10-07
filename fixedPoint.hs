{-# LANGUAGE UndecidableInstances #-}

module FixedPoint where

fix :: ((a -> b) -> a -> b) -> a -> b
fix f = f (fix f)

data Fix f = In { out :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  show (In f) = "(" ++ show f ++ ")"
