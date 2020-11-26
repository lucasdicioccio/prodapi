{-# LANGUAGE OverloadedStrings #-}
module Monitors.Counters (Counters(..), newCounters) where

import Prometheus as Prometheus

data Counters = Counters {
    executionStarted :: !(Vector Label1 Counter)
  , executionEnded :: !(Vector Label2 Counter)
  , stdoutLines :: !(Vector Label1 Counter)
 }

newCounters :: IO Counters
newCounters =
  Counters
    <$> counts1 "executions_start"
    <*> counts2 "executions_end"
    <*> counts1 "stdout_lines"
  where
    counts1 k =
      Prometheus.register
      $ Prometheus.vector "cmd"
      $ Prometheus.counter (Prometheus.Info k "")

    counts2 k =
      Prometheus.register
      $ Prometheus.vector ("cmd", "exit")
      $ Prometheus.counter (Prometheus.Info k "")
