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
import qualified Prod.Discovery
import Prod.Tracer

type Api = "hello-world" :> Get '[JSON] Text

data Track = WatchdogTrack (Prod.Watchdog.Track)
  | DiscoveryTrack (Prod.Discovery.DNSTrack)
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
  }

readDiscoveredHosts :: Runtime -> IO [Host]
readDiscoveredHosts = fmap (fromMaybe [] . toMaybe) . readCurrent . discovery

initRuntime :: T -> IO Runtime
initRuntime tracer = Runtime
  <$> newCounters
  <*> helloWatchdog
  <*> fileTouchWatchdog "./example-prodapi-watchdog" silent 5000000 
  <*> dnsA (contramap DiscoveryTrack tracer) "dicioccio.fr"

serve :: Runtime -> Handler Text
serve runtime = do
  liftIO $ timeIt helloes (counters runtime) $ do
    threadDelay =<< randomRIO (100, 10000)
    pure "hello world"
