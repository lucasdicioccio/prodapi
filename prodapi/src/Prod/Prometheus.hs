{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prod.Prometheus
  ( handlePrometheus,
    PrometheusApi,
    initPrometheus,
    inc,
    obs,
    timeIt,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Network.HTTP.Media as M
import Prometheus (Counter, Vector, exportMetricsAsText, register)
import qualified Prometheus as Prometheus
import Prometheus.Metric.GHC (GHCMetrics, ghcMetrics)
import Servant
import Servant.Server (Handler, Server)

newtype PrometheusResult = PrometheusResult {toLBS :: ByteString}

instance MimeRender PlainText PrometheusResult where
  mimeRender _ = toLBS

type PrometheusApi =
  Summary "Prometheus metrics"
    :> "metrics"
    :> Get '[PlainText] PrometheusResult

handlePrometheus :: Server PrometheusApi
handlePrometheus = handleMetrics
  where
    handleMetrics :: Handler PrometheusResult
    handleMetrics = liftIO $ fmap PrometheusResult exportMetricsAsText

initPrometheus :: IO GHCMetrics
initPrometheus = register ghcMetrics

inc ::
  MonadIO m =>
  (a -> Vector Text Counter) ->
  Text ->
  a ->
  m ()
inc f s cnts =
  liftIO $ Prometheus.withLabel (f cnts) s Prometheus.incCounter

obs ::
  MonadIO m =>
  (a -> Prometheus.Summary) ->
  Double ->
  a ->
  m ()
obs f v cnts =
  liftIO $ Prometheus.observe (f cnts) v

timeIt ::
  MonadIO m =>
  (a -> Prometheus.Summary) ->
  a ->
  m b ->
  m b
timeIt f cnts action = do
  t0 <- liftIO $ getCurrentTime
  !ret <- action
  t1 <- liftIO $ getCurrentTime
  obs f (fromRational $ toRational $ diffUTCTime t1 t0) cnts
  pure ret
