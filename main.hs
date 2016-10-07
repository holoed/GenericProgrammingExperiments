{-# LANGUAGE UndecidableInstances #-}

module Main where

import RecursionSchemes
import Zippers
import Expr
import Control.Monad.State hiding (modify)

sample :: Exp
sample = app (app (lit 1) (lit 2)) (app (lit 3) (lit 4))

alg :: Ann ExpF (Loc ExpF) (State (Maybe (Loc ExpF)) Exp) -> State (Maybe (Loc ExpF)) Exp
alg (Ann p (Let n e1 e2)) = do
  e1' <- e1
  e2' <- e2
--  _ <- put (Just p)
  return (leT n e1' e2')
alg (Ann _ (Lam n e)) = do
  e' <- e
  return (lam n e')
alg (Ann p (App e1 e2)) = do
  e1' <- e1
  e2' <- e2
--  _ <- put (Just p)
  return (app e1' e2')
alg (Ann _ (Var x)) = return (var x)
alg (Ann p (Lit v)) = do
  _ <- put (Just p)
  return (lit v)

transformed :: Maybe (Loc ExpF)
transformed = execState (cataRec alg (locations sample)) Nothing

sampleZipped :: Loc ExpF
sampleZipped = root sample

downOnce :: Maybe (Loc ExpF)
downOnce = moveDown 0 sampleZipped

downTwice :: Maybe (Loc ExpF)
downTwice =  downOnce >>= moveDown 0

modifyOnce :: Maybe (Loc ExpF)
modifyOnce = fmap (modify (\_ -> lit 43)) downTwice

tree :: Maybe Exp
tree = fmap extract (modifyOnce >>= moveUp >>= moveUp)

main :: IO ()
main = print transformed
