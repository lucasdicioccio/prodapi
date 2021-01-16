-- https://www.youtube.com/watch?v=qzOQOmmkKEM&feature=emb_logo

module Prod.Tracer (Tracer(..), Contravariant(..), traceBoth) where

import Data.Functor.Contravariant

newtype Tracer m a = Tracer { runTracer :: (a -> m ()) }

instance Contravariant (Tracer m) where
  contramap f (Tracer g) = Tracer (g . f)

-- | If you are given two tracers and want to pass both.
-- Composition occurs in sequence.
traceBoth :: (Applicative m) => Tracer m a -> Tracer m a -> Tracer m a
traceBoth (Tracer f1) (Tracer f2) = Tracer (\a -> f1 a *> f2 a)
