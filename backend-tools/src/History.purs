
-- | Performance improvements:
-- - fuse PromData away
-- - use Array as backing
module History where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List as List
import Data.Array as Array
import Data.Unfoldable (class Unfoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set

import Parsing.Prometheus (PromDoc, Line(..), Labels, MetricName, MetricValue)

type PromData =
  { metrics :: Map (Tuple MetricName Labels) MetricValue
  , helps :: Map MetricName String
  }

fromPromDoc :: PromDoc -> PromData
fromPromDoc metrics =
  { metrics: Map.fromFoldable $ List.catMaybes $ map toMetric metrics
  , helps: Map.fromFoldable $ List.catMaybes $ map toHelp metrics
  }
  where
    toMetric (MetricLine n lbls val _) = let key = Tuple n lbls in Just $ Tuple key val
    toMetric _                         = Nothing
    toHelp (HelpLine n s)              = Just (Tuple n s)
    toHelp _                           = Nothing

type HistoryKey = Tuple MetricName Labels
type HistoryData = Array (Maybe Number)

type History =
  { knownKeys :: Set HistoryKey
  , timeseriesData :: Map HistoryKey HistoryData
  }

emptyHistory :: History
emptyHistory = { knownKeys: Set.empty , timeseriesData: Map.empty }

historyKeys :: forall f. Unfoldable f => History -> f HistoryKey
historyKeys h = Set.toUnfoldable $ h.knownKeys

hdToList :: forall f. Unfoldable f => HistoryData -> f (Maybe Number)
hdToList = Array.toUnfoldable

lookupHistory :: HistoryKey -> History -> Maybe HistoryData
lookupHistory key h = Map.lookup key h.timeseriesData

updateHistory :: Int -> Maybe PromDoc -> History -> History
updateHistory n doc =
  updateHistory' n (map fromPromDoc doc)

updateHistory' :: Int -> Maybe PromData -> History -> History
updateHistory' _ Nothing h = h
updateHistory' histlen (Just promdata) h =
  h { knownKeys = allKeys
    , timeseriesData = map (Array.take histlen) wholeData
    }
  where
    recentKeys = Map.keys promdata.metrics
    allKeys = h.knownKeys `Set.union` recentKeys

    newData = Map.difference promdata.metrics h.timeseriesData
    agedData = Map.difference h.timeseriesData promdata.metrics
    updatedData = Map.intersectionWith append h.timeseriesData promdata.metrics

    wholeData = Map.unions
      [ updatedData
      , map (Array.singleton <<< Just) newData
      , map (Array.cons Nothing) agedData
      ]

    append xs x = Array.cons (Just x) xs
