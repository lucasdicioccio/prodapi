{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NumericUnderscores #-}

module Prod.Background
  ( BackgroundVal,
    MicroSeconds,
    background,
    backgroundLoop,
    kill,
    readBackgroundVal,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A value that is coupled to an async in charge of updating the value.
data BackgroundVal a
  = forall r.
    BackgroundVal
      { -- | a transformation to apply to the background val, allows to turn the IORef into a functor
        transform :: r -> a,
        -- | a mutable reference for the latest value
        currentValue :: IORef r,
        -- | a background task responsible for updating the value, implementations should guarantee that once the Aync () is cancelled, currentValue is never updated
        backgroundTask :: Async ()
      }

instance Functor BackgroundVal where
  fmap g (BackgroundVal f ioref task) =
    BackgroundVal (g . f) ioref task

-- | Starts a background task continuously updating a value.
background ::
  -- | initial state
  b ->
  -- | initial value
  a ->
  -- | state-influenced task
  (b -> IO (a, b)) ->
  IO (BackgroundVal a)
background initState defaultValue task = do
  ref <- newIORef defaultValue
  BackgroundVal id ref <$> async (loop ref initState)
  where
    loop ref st0 = do
      (val, st1) <- task st0
      seq val $ writeIORef ref val
      seq st1 $ loop ref st1

-- | Fantom type for annotating Int.
type MicroSeconds n = n

-- | Starts a background task continuously updating a value at a periodic interval.
-- This is implemented by interspersing a threadDelay before the task and calling background and hiding the 'state-passing' arguments.
backgroundLoop ::
  -- | initial value
  a ->
  -- | periodic task
  IO a ->
  -- | wait period between two executions
  MicroSeconds Int ->
  IO (BackgroundVal a)
backgroundLoop defaultValue task usecs = do
  background () defaultValue (\_ -> threadDelay usecs >> fmap adapt task)
  where
    adapt x = (x, ())

-- | Kills the watchdog by killing the underlying async.
kill :: MonadIO m => BackgroundVal a -> m ()
kill = liftIO . cancel . backgroundTask

-- | Kills the watchdog by killing the underlying async.
readBackgroundVal :: MonadIO m => BackgroundVal a -> m a
readBackgroundVal (BackgroundVal f ioref _) =
  fmap f $ liftIO $ readIORef ioref
