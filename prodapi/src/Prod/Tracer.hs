-- https://www.youtube.com/watch?v=qzOQOmmkKEM&feature=emb_logo

module Prod.Tracer (
  Tracer(..),
  silent,
  traceIf,
  traceBoth,
  -- * common utilities
  tracePrint,
  traceHPrint,
  traceHPut,
  encodeJSON,
  -- * re-exports
  Contravariant(..),
  Divisible(..),
  Decidable(..),
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString, hPut)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import System.IO (hPrint, Handle)

newtype Tracer m a = Tracer { runTracer :: (a -> m ()) }

instance Contravariant (Tracer m) where
  contramap f (Tracer g) = Tracer (g . f)

instance Applicative m => Divisible (Tracer m) where
  conquer = silent
  divide = traceSplit

instance Applicative m => Decidable (Tracer m) where
  lose _ = silent
  choose = tracePick

-- | Disable Tracing.
{-# INLINE silent #-}
silent :: (Applicative m) => Tracer m a
silent = Tracer (const $ pure ())

-- | Splits a tracer into two chunks that are run sequentially.
--
-- This name can be confusing but it has to be thought backwards for Contravariant logging:
-- We compose a target tracer from two tracers but we split the content of the trace.
--
-- Note that the split function may actually duplicate inputs (that's how traceBoth works).
{-# INLINEABLE traceSplit #-}
traceSplit :: (Applicative m) => (c -> (a,b)) -> Tracer m a -> Tracer m b -> Tracer m c
traceSplit split (Tracer f1) (Tracer f2) = Tracer (go . split)
    where
      go (b,c) = f1 b *> f2 c

-- | If you are given two tracers and want to pass both.
-- Composition occurs in sequence.
{-# INLINEABLE traceBoth #-}
traceBoth :: (Applicative m) => Tracer m a -> Tracer m a -> Tracer m a
traceBoth t1 t2 = traceSplit (\x -> (x,x)) t1 t2

-- | Picks a tracer based on the emitted object.
-- Example logic that can be built is traceIf that silent messages.
{-# INLINEABLE tracePick #-}
tracePick :: (Applicative m) => (c -> Either a b) -> Tracer m a -> Tracer m b -> Tracer m c
tracePick split (Tracer f1) (Tracer f2) = Tracer $ \a ->
  let e = split a
  in either f1 f2 e

-- | Filter by dynamically testing values.
{-# INLINEABLE traceIf #-}
traceIf :: (Applicative m) => (a -> Bool) -> Tracer m a -> Tracer m a
traceIf predicate t = tracePick (\x -> if predicate x then Left () else Right x) silent t

-- | A tracer that prints emitted events.
tracePrint :: (MonadIO m, Show a) => Tracer m a
tracePrint = Tracer (liftIO . print)

-- | A tracer that prints emitted to some handle.
traceHPrint :: (MonadIO m, Show a) => Handle -> Tracer m a
traceHPrint handle = Tracer (liftIO . hPrint handle)

-- | A tracer that puts some ByteString to some handle.
traceHPut :: (MonadIO m) => Handle -> Tracer m ByteString
traceHPut handle = Tracer (liftIO . hPut handle)

-- | A conversion encoding values to JSON.
{-# INLINE encodeJSON #-}
encodeJSON :: (ToJSON a) => Tracer m ByteString -> Tracer m a
encodeJSON = contramap encode
