-- https://www.youtube.com/watch?v=qzOQOmmkKEM&feature=emb_logo

module Prod.Tracer where

import Data.Functor.Contravariant

newtype Tracer m a = Tracer { trace :: (a -> m ()) }

instance Contravariant (Tracer m) where
  contramap f (Tracer g) = Tracer (g . f)

