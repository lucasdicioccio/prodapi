
module Charting.Charts where

import Parsing.Prometheus
import Data.List (List)
import Data.List as List
import Data.Tuple (Tuple(..))

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

specKeys :: ChartSpec -> List (Tuple MetricName Labels)
specKeys (SingleTimeSeries _ _ n lbls) = List.singleton (Tuple n lbls)
specKeys (MultiTimeSeries _ xs) = List.concatMap specKeys xs

specIndex :: ChartSpec -> Int
specIndex (SingleTimeSeries n _ _ _) = n
specIndex (MultiTimeSeries n _) = n
