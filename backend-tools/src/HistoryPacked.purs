
-- | Performance improvements:
-- - fuse PromData away
-- - use Array as backing
module HistoryPacked where

import Prelude
import Effect (Effect)
import Data.Foldable (foldM)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List as List
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set

import Parsing.Prometheus (PromDoc, Line(..), Labels, MetricName, MetricValue)

import Data.ArrayBuffer.Types (Float64Array)
import Data.ArrayBuffer.Typed as AB

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
type HistoryDataInternal = Float64Array
type HistoryData = Array Number

type History =
  { knownKeys :: Set HistoryKey
  , timeseriesData :: Map HistoryKey HistoryDataInternal
  , ptr :: Int
  }

historyLen :: Int
historyLen = 100

emptyHistory :: History
emptyHistory = { knownKeys: Set.empty , timeseriesData: Map.empty , ptr: 0}

historyKeys :: forall f. Unfoldable f => History -> f HistoryKey
historyKeys h = Set.toUnfoldable $ h.knownKeys

toArrayMap :: History -> Effect (Map HistoryKey (Array Number))
toArrayMap h = traverse AB.toArray h.timeseriesData

updateHistory' :: Maybe PromData -> History -> Effect History
updateHistory' Nothing h = pure h
updateHistory' (Just promdata) h = do
  wholeData <- updateWhole
  pure h { knownKeys = allKeys
         , timeseriesData = wholeData
         , ptr = (h.ptr + 1) `mod` historyLen
         }
  where
    recentKeys = Map.keys promdata.metrics
    allKeys = h.knownKeys `Set.union` recentKeys

    updateWhole :: Effect (Map HistoryKey HistoryDataInternal)
    updateWhole = foldM runUpdate h.timeseriesData allKeys

    runUpdate :: Map HistoryKey HistoryDataInternal -> HistoryKey -> Effect (Map HistoryKey HistoryDataInternal)
    runUpdate map0 key =
      case Tuple (Map.lookup key promdata.metrics) (Map.lookup key h.timeseriesData) of
        Tuple Nothing Nothing ->
          pure map0 -- note: should not be possible since we iterate on known keys

        Tuple Nothing (Just buf) -> do
          _ <- AB.set buf (Just h.ptr) [-42.0]
          pure map0

        Tuple (Just newVal) Nothing -> do
          buf <- newBuf
          _ <- AB.set buf (Just h.ptr) [newVal]
          pure $ Map.insert key buf map0

        Tuple (Just newVal) (Just buf) -> do
          _ <- AB.set buf (Just h.ptr) [newVal]
          pure map0

newBuf :: Effect HistoryDataInternal
newBuf = AB.empty historyLen
