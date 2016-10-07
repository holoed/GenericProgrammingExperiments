{-# LANGUAGE UndecidableInstances #-}

module Main where

import RecursionSchemes
import Zippers
import Expr
import Control.Monad.State hiding (modify)
import ExprUtils

sample :: Exp
sample = app (app (lit 1) (lit 2)) (app (lit 3) (lit 4))

type Carrier = State (Maybe (Loc ExpF)) Exp

alg :: (F Carrier -> Carrier) -> (F Carrier -> Carrier)
alg _ (Ann p (Lit 3))  = do
  _ <- put (Just p)
  return (lit 3)
alg k x = k x

transformed :: Maybe (Loc ExpF)
transformed = execState (cataRec (alg exprAlg) (locations sample)) Nothing

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
