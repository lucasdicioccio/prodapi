-- https://www.youtube.com/watch?v=qzOQOmmkKEM&feature=emb_logo

module Prod.Tracer (Tracer(..), Contravariant(..), silent, traceIf, traceBoth) where

import Data.Functor.Contravariant

newtype Tracer m a = Tracer { runTracer :: (a -> m ()) }

instance Contravariant (Tracer m) where
  contramap f (Tracer g) = Tracer (g . f)

-- | Disable Tracing.
{-# INLINE silent #-}
silent :: (Applicative m) => Tracer m a
silent = Tracer (const $ pure ())

-- | Filter by dynamically testing values.
{-# INLINEABLE traceIf #-}
traceIf :: (Applicative m) => (a -> Bool) -> Tracer m a -> Tracer m a
traceIf predicate (Tracer f) = Tracer (\a -> if predicate a then f a else pure ())

-- | If you are given two tracers and want to pass both.
-- Composition occurs in sequence.
{-# INLINEABLE traceBoth #-}
traceBoth :: (Applicative m) => Tracer m a -> Tracer m a -> Tracer m a
traceBoth (Tracer f1) (Tracer f2) = Tracer (\a -> f1 a *> f2 a)
