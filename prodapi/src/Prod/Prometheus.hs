{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prod.Prometheus (
    handlePrometheus,
    PrometheusApi,
    CORSAllowOrigin (..),
    PrometheusResult (..),
    initPrometheus,
    inc,
    obs,
    timeIt,
)
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Coerce (coerce)
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
        :> Get '[PlainText] (Headers '[Header "Access-Control-Allow-Origin" CORSAllowOrigin] PrometheusResult)

newtype CORSAllowOrigin = CORSAllowOrigin Text
    deriving (ToHttpApiData)

handlePrometheus :: CORSAllowOrigin -> Server PrometheusApi
handlePrometheus corsAllow = handleMetrics
  where
    handleMetrics :: Handler (Headers '[Header "Access-Control-Allow-Origin" CORSAllowOrigin] PrometheusResult)
    handleMetrics = do
        metrics <- liftIO $ exportMetricsAsText
        pure $ addHeader (coerce corsAllow) $ PrometheusResult metrics

initPrometheus :: IO GHCMetrics
initPrometheus = register ghcMetrics

inc ::
    (MonadIO m) =>
    (a -> Vector Text Counter) ->
    Text ->
    a ->
    m ()
inc f s cnts =
    liftIO $ Prometheus.withLabel (f cnts) s Prometheus.incCounter

obs ::
    (MonadIO m) =>
    (a -> Prometheus.Summary) ->
    Double ->
    a ->
    m ()
obs f v cnts =
    liftIO $ Prometheus.observe (f cnts) v

timeIt ::
    (MonadIO m) =>
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
