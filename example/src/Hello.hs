{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Hello where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Prod.Background as Background
import Prod.Discovery (Discovery, Host, dnsA, readCurrent, toMaybe)
import qualified Prod.Discovery as Discovery
import qualified Prod.Healthcheck as Healthcheck
import Prod.Prometheus (timeIt)
import Prod.Tracer
import Prod.Watchdog (Watchdog, WatchdogResult (..), basicWatchdog, fileTouchWatchdog)
import qualified Prod.Watchdog
import qualified Prometheus as Prometheus
import Servant
import Servant.Server
import System.Random (randomRIO)

type Api = "hello-world" :> Get '[JSON] Text

data Track
    = DiscoveryTrack (Discovery.DNSTrack [Host])
    | HealthcheckTrack (Healthcheck.Namespace, Healthcheck.Track)
    deriving (Show)

type T = Tracer IO Track

data Counters = Counters
    { helloes :: Prometheus.Summary
    , routedQueries :: Prometheus.Counter
    , fallbackProxiedQueries :: Prometheus.Counter
    , nobackendProxiedQueries :: Prometheus.Counter
    }

newCounters :: IO Counters
newCounters =
    Counters
        <$> mkHelloesSummary
        <*> mkRoutedCounter
        <*> mkFallbackCounter
        <*> mkNoFallbackCounter
  where
    mkHelloesSummary =
        let name = "helloes_durations"
            help = "measure time to say hello"
         in Prometheus.register $
                Prometheus.summary
                    (Prometheus.Info name help)
                    Prometheus.defaultQuantiles
    mkRoutedCounter =
        Prometheus.register $
            Prometheus.counter (Prometheus.Info "routed_queries" "number of queries routed")
    mkFallbackCounter =
        Prometheus.register $
            Prometheus.counter (Prometheus.Info "fallback_queries" "number of queries using fallback")
    mkNoFallbackCounter =
        Prometheus.register $
            Prometheus.counter (Prometheus.Info "fallback_missing_queries" "number of queries missing a fallback")

helloWatchdog :: IO (Watchdog ())
helloWatchdog = do
    counter <-
        Prometheus.register $
            Prometheus.vector "status" $
                Prometheus.counter (Prometheus.Info "watchdog_hello" "continuously working")
    basicWatchdog counter silent 500000 (pure $ Success ())

data Runtime = Runtime
    { counters :: Counters
    , watchdog1 :: Watchdog ()
    , watchdog2 :: Watchdog UTCTime
    , discovery1 :: Discovery [Host]
    , discovery2 :: Discovery [Host]
    , healthchecks :: Healthcheck.Runtime
    }

readDiscoveredHosts1 :: Runtime -> IO [Host]
readDiscoveredHosts1 = fmap (fromMaybe [] . toMaybe) . readCurrent . discovery1

initRuntime :: T -> IO Runtime
initRuntime tracer = do
    healthchecker <- Healthcheck.initRuntime (contramap HealthcheckTrack tracer)
    -- _ <- Background.backgroundLoop tracePrint () (Healthcheck.cancelDeadChecks healthchecker) 1000000
    let healthCheckDiscoveredHosts ns = contramap (fmap port80) $ Tracer $ \discmsg -> Healthcheck.withSpace healthchecker ns (\space -> (Healthcheck.addChecksFromDNSDiscovery space discmsg))
    let discoveryTracker = contramap DiscoveryTrack tracer
    Runtime
        <$> newCounters
        <*> helloWatchdog
        <*> fileTouchWatchdog "./example-prodapi-watchdog" silent 5000000
        <*> dnsA (traceBoth discoveryTracker (healthCheckDiscoveredHosts "dyncioccio")) "laptop.dyn.dicioccio.fr"
        <*> dnsA (traceBoth discoveryTracker (healthCheckDiscoveredHosts "service3")) "dicioccio.fr"
        <*> pure healthchecker
  where
    port80 hosts = [(host, 80) | host <- hosts]

serve :: Runtime -> Handler Text
serve runtime = do
    liftIO $ timeIt helloes (counters runtime) $ do
        threadDelay =<< randomRIO (100, 10000)
        pure "hello world"
