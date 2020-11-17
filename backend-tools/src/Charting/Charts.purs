
module Charting.Charts where

import Parsing.Prometheus

data ChartDisplayMode
  = Samples
  | DiffSamples
  | Smooth

data ChartSpec
  = SingleTimeSeries Int ChartDisplayMode MetricName Labels

specIndex :: ChartSpec -> Int
specIndex (SingleTimeSeries n _ _ _) = n

