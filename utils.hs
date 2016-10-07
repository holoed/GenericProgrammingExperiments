module Utils where

  -- left-to-right state transformer
  newtype StateL s a = StateL { runStateL :: s -> (s, a) }

  instance Functor (StateL s) where
      fmap f (StateL k) = StateL $ \ s -> let (s', v) = k s in (s', f v)

  instance Applicative (StateL s) where
      pure x = StateL (\ s -> (s, x))
      StateL kf <*> StateL kv = StateL $ \ s ->
          let (s', f) = kf s
              (s'', v) = kv s'
          in (s'', f v)

  -- |The 'mapAccumL' function behaves like a combination of 'fmap'
  -- and 'foldl'; it applies a function to each element of a structure,
  -- passing an accumulating parameter from left to right, and returning
  -- a final value of this accumulator together with the new structure.
  mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
  mapAccumL f s t = runStateL (traverse (StateL . flip f) t) s
