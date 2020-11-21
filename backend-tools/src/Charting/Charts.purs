
module Charting.Charts where

import Parsing.Prometheus
import Data.List (List)
import Data.List as List
import Data.Tuple (Tuple(..))

data ChartDisplayMode
  = Samples
  | DiffSamples

cycleDisplayMode :: ChartDisplayMode -> ChartDisplayMode
cycleDisplayMode Samples = DiffSamples
cycleDisplayMode DiffSamples = Samples

data ChartSpec
  = TimeSeries Int ChartDisplayMode MetricName Labels

specKeys :: ChartSpec -> List (Tuple MetricName Labels)
specKeys (TimeSeries _ _ n lbls) = List.singleton (Tuple n lbls)

specIndex :: ChartSpec -> Int
specIndex (TimeSeries n _ _ _) = n
