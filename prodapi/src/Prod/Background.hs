{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NumericUnderscores #-}

module Prod.Background
  ( BackgroundVal,
    MicroSeconds,
    background,
    backgroundLoop,
    kill,
    readBackgroundVal,
    Track(..)
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Prod.Tracer (Tracer(..))

import GHC.Stack (HasCallStack, callStack, CallStack)

data Track =
    Init
  | RunStart
  | RunDone
  | Kill CallStack

-- | A value that is coupled to an async in charge of updating the value.
data BackgroundVal a
  = forall r.
    BackgroundVal
      { -- | a transformation to apply to the background val, allows to turn the IORef into a functor
        transform :: r -> a,
        -- | a mutable reference for the latest value
        currentValue :: IORef r,
        -- | a background task responsible for updating the value, implementations should guarantee that once the Aync () is cancelled, currentValue is never updated
        backgroundTask :: Async (),
        tracer :: Tracer IO Track
      }

traceBkg :: MonadIO m => BackgroundVal a -> Track -> m ()
traceBkg b t = liftIO $ (runTracer $ tracer b) t

instance Functor BackgroundVal where
  fmap g (BackgroundVal f ioref task tracer) =
    BackgroundVal (g . f) ioref task tracer

-- | Starts a background task continuously updating a value.
background ::
  Tracer IO Track ->
  -- | initial state
  b ->
  -- | initial value
  a ->
  -- | state-influenced task
  (b -> IO (a, b)) ->
  IO (BackgroundVal a)
background tracer initState defaultValue task = do
  trace Init
  ref <- newIORef defaultValue
  BackgroundVal id ref <$> async (loop ref initState) <*> pure tracer
  where
    trace = runTracer tracer
    loop ref st0 = do
      trace (RunStart)
      (val, st1) <- task st0
      trace (RunDone)
      seq val $ writeIORef ref val
      seq st1 $ loop ref st1

-- | Fantom type for annotating Int.
type MicroSeconds n = n

-- | Starts a background task continuously updating a value at a periodic interval.
-- This is implemented by interspersing a threadDelay before the task and calling background and hiding the 'state-passing' arguments.
backgroundLoop ::
  Tracer IO Track ->
  -- | initial value
  a ->
  -- | periodic task
  IO a ->
  -- | wait period between two executions
  MicroSeconds Int ->
  IO (BackgroundVal a)
backgroundLoop tracer defaultValue task usecs = do
  background tracer () defaultValue (\_ -> threadDelay usecs >> fmap adapt task)
  where
    adapt x = (x, ())

-- | Kills the watchdog by killing the underlying async.
kill :: (HasCallStack, MonadIO m) => BackgroundVal a -> m ()
kill bkg = do
  traceBkg bkg (Kill callStack)
  liftIO $ cancel . backgroundTask $ bkg

-- | Kills the watchdog by killing the underlying async.
readBackgroundVal :: MonadIO m => BackgroundVal a -> m a
readBackgroundVal (BackgroundVal f ioref _ _) =
  fmap f $ liftIO $ readIORef ioref
