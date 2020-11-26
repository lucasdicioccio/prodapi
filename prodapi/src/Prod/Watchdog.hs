
module Prod.Watchdog where

import Prod.Background
import Prometheus (Vector, Label1, Counter)
import qualified Prometheus as Prometheus

data WatchdogResult a
  = Skipped
  | Success a
  | Failed
  deriving (Show, Ord, Eq)

data Watchdog a = Watchdog {
    backgroundVal :: BackgroundVal (WatchdogResult a)
  }

basicWatchdog :: Vector Label1 Counter -> MicroSeconds Int -> IO (WatchdogResult a) -> IO (Watchdog a)
basicWatchdog counter delay action =
    Watchdog <$> background () Skipped go delay
  where
    go _ = do
          res <- action
          let label = case res of
                Success _ -> "success"
                Failed    -> "failed"
                Skipped   -> "skipped"
          Prometheus.withLabel counter label Prometheus.incCounter
          pure (res, ())
