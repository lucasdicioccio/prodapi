
module Charting.Charts where

import Parsing.Prometheus
import Data.List (List)

data ChartDisplayMode
  = Samples
  | DiffSamples
  | Smooth

cycleDisplayMode :: ChartDisplayMode -> ChartDisplayMode
cycleDisplayMode Samples = DiffSamples
cycleDisplayMode DiffSamples = Smooth
cycleDisplayMode Smooth = Samples

data ChartSpec
  = SingleTimeSeries Int ChartDisplayMode MetricName Labels
  | MultiTimeSeries Int (List ChartSpec)

specIndex :: ChartSpec -> Int
specIndex (SingleTimeSeries n _ _ _) = n
specIndex (MultiTimeSeries n _) = n
