
module Prod.Watchdog where

import Data.Text (Text)
import qualified Data.Text as Text
import Prod.Background
import Prometheus (Vector, Label1, Label2, Counter)
import qualified Prometheus as Prometheus
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Exception.Base (IOException, catch)
import System.Directory (setModificationTime)

data WatchdogResult a
  = Skipped
  | Success a
  | Failed
  deriving (Show, Ord, Eq)

data Watchdog a = Watchdog {
    backgroundVal :: BackgroundVal (WatchdogResult a)
  }

watchdog
  :: (Prometheus.Label label)
  => Vector label Counter
  -> (WatchdogResult a -> label)
  -> MicroSeconds Int
  -> IO (WatchdogResult a)
  -> IO (Watchdog a)
watchdog counters mkLabel delay action =
    Watchdog <$> background () Skipped go delay
  where
    go _ = do
          res <- action
          Prometheus.withLabel counters (mkLabel res) Prometheus.incCounter
          pure (res, ())

-- | Basic watchdog with a vector metric.
-- The input vector label is set with success|failed|skipped depending on the WatchdogResult.
basicWatchdog
  :: Vector Label1 Counter
  -> MicroSeconds Int
  -> IO (WatchdogResult a)
  -> IO (Watchdog a)
basicWatchdog counters delay action =
    watchdog counters basicLabel delay action

basicLabel :: WatchdogResult a -> Label1
basicLabel res = case res of
  Success _ -> "success"
  Failed    -> "failed"
  Skipped   -> "skipped"

-- | Touches a file periodically, using setModificationTime
fileTouchWatchdog
  :: FilePath
  -> MicroSeconds Int
  -> IO (Watchdog UTCTime)
fileTouchWatchdog path delay = do
    let mkLabel res = (basicLabel res, Text.pack path)
    watchdog fileTouchWatchdogCounter mkLabel delay io
  where
    handleIOException :: IOException -> IO (WatchdogResult UTCTime)
    handleIOException _ = pure $ Failed
    io = do
      now <- getCurrentTime
      let touchFile = setModificationTime path now *> pure (Success now)
      touchFile `catch` handleIOException

{-# NOINLINE fileTouchWatchdogCounter #-}
fileTouchWatchdogCounter :: Vector Label2 Counter
fileTouchWatchdogCounter =
  Prometheus.unsafeRegister
    $ Prometheus.vector ("status", "path")
    $ Prometheus.counter (Prometheus.Info "prodapi_watchdog_filetouch" "")
