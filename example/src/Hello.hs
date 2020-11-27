{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Hello where

import Servant
import Servant.Server
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Prod.Prometheus (timeIt)
import System.Random (randomRIO)
import qualified Prometheus as Prometheus
import Prod.Watchdog (basicWatchdog, Watchdog, WatchdogResult(..), fileTouchWatchdog)
import Data.Time.Clock (UTCTime)

type Api = "hello-world" :> Get '[JSON] Text

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
  basicWatchdog counter 500000 (pure $ Success ())

data Runtime = Runtime
  { counters :: Counters
  , watchdog1 :: Watchdog ()
  , watchdog2 :: Watchdog UTCTime
  }

initRuntime :: IO Runtime
initRuntime = Runtime
  <$> newCounters
  <*> helloWatchdog
  <*> fileTouchWatchdog "./example-prodapi-watchdog" 5000000

serve :: Runtime -> Handler Text
serve runtime = do
  liftIO $ timeIt helloes (counters runtime) $ do
    threadDelay =<< randomRIO (100, 10000)
    pure "hello world"
