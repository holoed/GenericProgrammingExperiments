module ExprUtils where

import Expr
import Zippers

type F a = Ann ExpF (Loc ExpF) a

exprAlg :: Monad m => F (m Exp) -> m Exp
exprAlg (Ann _ (Let n e1 e2)) = do
  e1' <- e1
  e2' <- e2
  return (leT n e1' e2')
exprAlg (Ann _ (Lam n e)) = do
  e' <- e
  return (lam n e')
exprAlg (Ann _ (App e1 e2)) = do
  e1' <- e1
  e2' <- e2
  return (app e1' e2')
exprAlg (Ann _ (Var x)) = return (var x)
exprAlg (Ann _ (Lit v)) = return (lit v)
