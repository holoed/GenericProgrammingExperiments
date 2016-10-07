{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Expr where

import FixedPoint

data ExpF a = Var String
            | App a a
            | Lam String a
            | Lit Int
            | Let String a a
            deriving (Functor, Show, Foldable, Traversable)

type Exp = Fix ExpF

var :: String -> Exp
var x = In (Var x)

app :: Exp -> Exp -> Exp
app e1 e2 = In (App e1 e2)

lam :: String -> Exp -> Exp
lam s e = In (Lam s e)

lit :: Int -> Exp
lit x = In (Lit x)

leT :: String -> Exp -> Exp -> Exp
leT s v b = In (Let s v b)
