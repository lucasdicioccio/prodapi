
module History where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set

import Parsing.Prometheus (promDoc, PromDoc, Line(..), Labels, LabelPair, pairName, pairValue, MetricName, MetricValue)

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

type History =
  { allKeys :: Set HistoryKey
  , timeseriesData :: Map HistoryKey (List (Maybe Number))
  }

emptyHistory :: History
emptyHistory = { allKeys: Set.empty , timeseriesData: Map.empty }

lookupHistory :: HistoryKey -> History -> Maybe (List (Maybe Number))
lookupHistory key h = map List.reverse $ Map.lookup key h.timeseriesData

updateHistory :: Int -> Maybe PromDoc -> History -> History
updateHistory n doc =
  updateHistory' n (map fromPromDoc doc)

updateHistory' :: Int -> Maybe PromData -> History -> History
updateHistory' _ Nothing h = h
updateHistory' histlen (Just promdata) h =
  h { allKeys = unionKeys
    , timeseriesData = map (List.take histlen) wholeData
    }
  where
    recentKeys = Map.keys promdata.metrics
    unionKeys = h.allKeys `Set.union` recentKeys

    newData = Map.difference promdata.metrics h.timeseriesData
    agedData = Map.difference h.timeseriesData promdata.metrics
    updatedData = Map.intersectionWith append h.timeseriesData promdata.metrics

    wholeData = Map.unions
      [ updatedData
      , map (List.singleton <<< Just) newData
      , map (flip List.snoc Nothing) agedData
      ]

    append xs x = List.snoc xs (Just x)
