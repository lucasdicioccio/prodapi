{-# LANGUAGE KindSignatures #-}

module Prod.Reports
  ( ReportsApi,
    Report(..),
    countReports,
    Runtime,
    initRuntime,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (.=), FromJSON (..), withObject, ToJSON(..), object)
import GHC.TypeLits (Symbol)
import qualified Prometheus as Prometheus
import Servant ((:>), JSON, Post, ReqBody, Summary)
import Servant.Server (Handler)
import Prod.Tracer

-- | Some minimal report wrapper.
-- Has low expectations on the client.
data Report a = Report {posixTime :: Int, backoff :: Int, events :: [a]}

instance FromJSON a => FromJSON (Report a) where
  parseJSON = withObject "Report" $ \o ->
    Report <$> o .: "t" <*> o .: "b" <*> o .: "es"
instance ToJSON a => ToJSON (Report a) where
  toJSON (Report t b es) = object [ "t" .= t, "b" .= b ,  "es" .= es ]

type ReportsApi a =
  Summary "receives and acknowledge some reports"
    :> "reports"
    :> ReqBody '[JSON] (Report a)
    :> Post '[JSON] Int

-- | A set of counters tracking .
data Counters
  = Counters
      { reports_count :: !Prometheus.Counter,
        reported_events :: !Prometheus.Counter,
        reported_sizes :: !Prometheus.Summary,
        reported_backoffs :: !Prometheus.Summary
      }

newCounters :: IO Counters
newCounters =
  Counters
    <$> counts "reports"
    <*> counts "report_events"
    <*> summary "report_sizes"
    <*> summary "report_backoffs"
  where
    counts k =
      Prometheus.register $
        Prometheus.counter (Prometheus.Info k "")
    summary k =
      Prometheus.register $
        Prometheus.summary (Prometheus.Info k "") Prometheus.defaultQuantiles

-- | A default runtime for the `dropReports` route.
data Runtime a = Runtime { counters :: !Counters , tracer :: !(Tracer IO (Report a)) }

initRuntime :: Tracer IO (Report a) -> IO (Runtime a)
initRuntime tracer = Runtime <$> newCounters <*> pure tracer

-- | Count and drop reports.
countReports :: Runtime a -> Report a -> Handler Int
countReports (Runtime counters tracer) report = do
  let size = length (events report)
  let dsize = fromIntegral size
  liftIO $ do
    runTracer tracer $ report
    Prometheus.incCounter (reports_count counters)
    Prometheus.addCounter (reported_events counters) dsize
    Prometheus.observe (reported_sizes counters) dsize
    Prometheus.observe (reported_backoffs counters) (fromIntegral $ backoff report)
  pure size
