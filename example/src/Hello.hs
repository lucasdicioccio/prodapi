{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Hello where

import Data.Maybe (fromMaybe)
import Servant
import Servant.Server
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Prod.Prometheus (timeIt)
import System.Random (randomRIO)
import qualified Prometheus as Prometheus
import Prod.Watchdog (basicWatchdog, Watchdog, WatchdogResult(..), fileTouchWatchdog)
import qualified Prod.Watchdog
import Data.Time.Clock (UTCTime)
import Prod.Discovery (Discovery, dnsA, Host, toMaybe, readCurrent)
import qualified Prod.Background as Background
import qualified Prod.Discovery as Discovery
import qualified Prod.Healthcheck as Healthcheck
import Prod.Tracer

type Api = "hello-world" :> Get '[JSON] Text

data Track = DiscoveryTrack (Discovery.DNSTrack [Host])
  | HealthcheckTrack Healthcheck.Track
  deriving (Show)

type T = Tracer IO Track

data Counters = Counters
  { helloes :: Prometheus.Summary
  }

newCounters :: IO Counters
newCounters =
  Counters <$> mkHelloesSummary
  where
    mkHelloesSummary =
      let name = "helloes_durations" 
          help = "measure time to say hello"
      in Prometheus.register $
        Prometheus.summary
          (Prometheus.Info name help)
          Prometheus.defaultQuantiles

helloWatchdog :: IO (Watchdog ())
helloWatchdog = do
  counter <- Prometheus.register
    $ Prometheus.vector "status"
    $ Prometheus.counter (Prometheus.Info "watchdog_hello" "continuously working")
  basicWatchdog counter silent 500000 (pure $ Success ())

data Runtime = Runtime
  { counters :: Counters
  , watchdog1 :: Watchdog ()
  , watchdog2 :: Watchdog UTCTime
  , discovery :: Discovery [Host]
  , healthchecks :: Healthcheck.Runtime
  }

readDiscoveredHosts :: Runtime -> IO [Host]
readDiscoveredHosts = fmap (fromMaybe [] . toMaybe) . readCurrent . discovery

initRuntime :: T -> IO Runtime
initRuntime tracer = do
  healthchecker <- Healthcheck.initRuntime (contramap HealthcheckTrack tracer)
  let healthCheckDiscoveredHosts = contramap (fmap port80) $ Tracer $ Healthcheck.setChecksFromDNSDiscovery healthchecker
  let discoveryTracker = contramap DiscoveryTrack tracer
  Runtime
    <$> newCounters
    <*> helloWatchdog
    <*> fileTouchWatchdog "./example-prodapi-watchdog" silent 5000000 
    <*> dnsA (traceBoth discoveryTracker healthCheckDiscoveredHosts) "dicioccio.fr"
    <*> pure healthchecker

  where
    port80 hosts = [(host,80)|host<-hosts]

serve :: Runtime -> Handler Text
serve runtime = do
  liftIO $ timeIt helloes (counters runtime) $ do
    threadDelay =<< randomRIO (100, 10000)
    pure "hello world"
