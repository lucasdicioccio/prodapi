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

data Runtime = Runtime
  { counters :: Counters
  }

initRuntime :: IO Runtime
initRuntime = Runtime <$> newCounters

serve :: Runtime -> Handler Text
serve runtime = do
    liftIO $ timeIt helloes (counters runtime) $ do
      threadDelay =<< randomRIO (100, 10000)
      pure "hello world"
