{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Zippers where

import FixedPoint
import Data.Maybe
import Data.List (foldl')
import Utils

-- | A context node.
type Node f  =  Either (Fix f) (Path f)

-- | The context or path type. The invariant we must respect is that there is exactly
-- one child with the 'Right' constructor.
data Path f  =  Top
             |  Path { unPath :: f (Node f) }

-- | The zipper type itself, which encodes a locations in thre tree @Mu f@.
data Loc f   =  Loc { focus :: Fix f , path :: Path f }

instance Show (f (Node f)) => Show (Path f) where
  show Top = "Top"
  show x = show (unPath x)

instance (Show (f (Node f)), Show (f (Fix f)))  => Show (Loc f) where
  show x = show ("focus:" ++ show (focus x), "path:" ++ show (path x))

-- | Creates a zipper from a tree, with the focus at the root.
root :: Fix f -> Loc f
root t = Loc t Top

--------------------------------------------------------------------------------
-- * Manipulating the subtree at focus

-- | Extracts the subtree at focus. Synonym of 'focus'.
extract :: Loc f -> Fix f
extract = focus

-- | Replaces the subtree at focus.
replace :: Fix f -> Loc f -> Loc f
replace new loc = loc { focus = new }

-- | Modifies the subtree at focus.
modify :: (Fix f -> Fix f) -> Loc f -> Loc f
modify h loc = replace (h (focus loc)) loc

-- | Moves up.
moveUp :: Traversable f => Loc f -> Maybe (Loc f)
moveUp (Loc foc p) = case p of
  Top         -> Nothing
  Path nodes  ->
    case mpath of
      Nothing      -> error "moveUp: shouldn't happen"
      Just path'   -> Just $ case path' of
        Path nodes'    -> Loc (In foc') (Path nodes')
        Top            -> Loc (In foc') Top
    where
      (mpath,foc') = mapAccumL g Nothing nodes
      g old ei = case ei of
        Right  p'  -> (Just p'  , foc)
        Left   x  -> (old     , x  )

moveDown :: Traversable f => Int -> Loc f -> Maybe (Loc f)
moveDown pos (Loc foc p) = new where
  new = case mfoc' of
     Nothing    ->  Nothing
     Just foc'  ->  Just $ Loc foc' (Path nodes')
  ((mfoc',_),nodes')  =  mapAccumL g (Nothing,0) (out foc)
  g (old,j) x  =  if j==pos
    then  ((Just x  , j+1),  Right  p  )
    else  ((old     , j+1),  Left   x     )

moveToLoc :: Traversable f => [Int] -> Fix f -> Maybe (Loc f)
moveToLoc xs e = foldl' (\acc i -> acc >>= moveDown i) (Just (root e)) xs

unsafeMoveDown :: Traversable f => Int -> Loc f -> Loc f
unsafeMoveDown i x = fromMaybe (error "unsafeMoveDown: cannot move down") (moveDown i x)

-- | Type of annotations
data Ann f a b = Ann
  { attr  :: a           -- ^ the annotation
  , unAnn :: f b         -- ^ the original functor
  }
  deriving (Eq,Ord,Show, Functor)

-- | Annotated fixed-point type. Equivalent to @CoFree f a@
type Attr f a = Fix (Ann f a)

enumerateWith :: Traversable f => (Int -> a -> b) -> f a -> (Int, f b)
enumerateWith h = mapAccumL (\i x -> (i+1, h i x)) 0

enumerateWith_ :: Traversable f => (Int -> a -> b) -> f a -> f b
enumerateWith_ h = snd . enumerateWith h

-- | We attribute all nodes with a zipper focused at that location.
locations :: Traversable f => Fix f -> Attr f (Loc f)
locations tree = go (root tree) tree where
  go loc (In t) = In (Ann loc t') where
    t' = enumerateWith_ (\j x -> go (unsafeMoveDown j loc) x) t
