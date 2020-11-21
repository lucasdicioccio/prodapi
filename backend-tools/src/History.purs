
-- | Performance improvements:
-- - fuse PromData away
-- - use Array as backing
module History where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List as List
import Data.Array as Array
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set

import Parsing.Prometheus (PromDoc, Line(..), Labels, MetricName, MetricValue)

import Data.Array.ST as STA
import Data.Array.ST.Partial as STAP
import Partial.Unsafe (unsafePartial)

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
  , ptr :: Int
  }

historyLen :: Int
historyLen = 100

emptyHistory :: History
emptyHistory = { knownKeys: Set.empty , timeseriesData: Map.empty , ptr: 0}

historyKeys :: forall f. Unfoldable f => History -> f HistoryKey
historyKeys h = Set.toUnfoldable $ h.knownKeys

hdToList :: forall f. Unfoldable f => History -> HistoryData -> f (Maybe Number)
hdToList h xs =
  unfoldr f h.ptr
  where
    len = Array.length xs
    f i
      | i < h.ptr + len = Just (Tuple (unsafePartial (Array.unsafeIndex xs $ i `mod` len)) (i+1))
      | otherwise       = Nothing

lookupHistory :: HistoryKey -> History -> Maybe HistoryData
lookupHistory key h = Map.lookup key h.timeseriesData

updateHistory :: Int -> Maybe PromDoc -> History -> History
updateHistory n doc =
  updateHistory' n (map fromPromDoc doc)

updateHistory' :: Int -> Maybe PromData -> History -> History
updateHistory' _ Nothing h = h
updateHistory' histlen (Just promdata) h =
  h { knownKeys = allKeys
    , timeseriesData = wholeData
    , ptr = (h.ptr + 1) `mod` historyLen
    }
  where
    recentKeys = Map.keys promdata.metrics
    allKeys = h.knownKeys `Set.union` recentKeys

    newData = Map.difference promdata.metrics h.timeseriesData
    agedData = Map.difference h.timeseriesData promdata.metrics
    updatedData = Map.intersectionWith insert h.timeseriesData promdata.metrics

    wholeData = Map.unions
      [ updatedData
      , map (mutate newBuf <<< Just) newData
      , map (flip mutate Nothing) agedData
      ]

    insert :: Array (Maybe Number) -> Number -> Array (Maybe Number)
    insert xs n = mutate xs (Just n)


{- TODO: debug why the unsafeThaw doesn't work
    mutate :: Array (Maybe Number) -> Maybe Number -> Array (Maybe Number)
    mutate xs x = STA.run (do
      ary <- STA.unsafeThaw xs
      _ <- unsafePartial $ STAP.poke h.ptr x ary
      pure ary
      ) 
-}

    mutate :: Array (Maybe Number) -> Maybe Number -> Array (Maybe Number)
    mutate xs x = fromMaybe [] $ Array.updateAt h.ptr x xs

newBuf :: Array (Maybe Number)
newBuf = Array.replicate historyLen Nothing
