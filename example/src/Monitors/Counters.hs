{-# LANGUAGE OverloadedStrings #-}
module Monitors.Counters (Counters(..), newCounters) where

import qualified Prometheus as Prometheus

data Counters = Counters {
    executionStarted :: !Prometheus.Counter
  , executionEnded :: !Prometheus.Counter
  , stdoutLines :: !Prometheus.Counter
 }

newCounters :: IO Counters
newCounters =
  Counters
    <$> counts "executions_start"
    <*> counts "executions_end"
    <*> counts "stdout_lines"
  where
    counts k =
      Prometheus.register $
        Prometheus.counter (Prometheus.Info k "")
