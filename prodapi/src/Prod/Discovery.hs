{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for performing service (endpoints) discovery.
module Prod.Discovery where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Prometheus (Vector, Label3, Gauge, Counter)
import qualified Prometheus as Prometheus
import Prod.Background (BackgroundVal, background, MicroSeconds, readBackgroundVal)
import System.Process.ByteString (readProcessWithExitCode)

data Result a
  = NotAsked
  | Asked UTCTime
  | Found UTCTime a
  deriving (Functor)

toMaybe :: Result a -> Maybe a
toMaybe (Found _ a) = Just a
toMaybe _           = Nothing

data Discovery a = Discovery (BackgroundVal (Result a))
  deriving (Functor)

readCurrent :: Discovery a -> IO (Result a)
readCurrent (Discovery b) = readBackgroundVal b

type Host = Text

dnsA :: Host -> IO (Discovery [Host])
dnsA hostname = dig "A" $ Text.unpack hostname

dnsAAAA :: Host -> IO (Discovery [Host])
dnsAAAA hostname = dig "AAAA" $ Text.unpack hostname

dig :: String -> String -> IO (Discovery [Host])
dig typ target = cmdOut "dig" ["+short",typ,target] "" tenSecs [] replaceHosts trigger
  where
    replaceHosts :: [Host] -> ByteString -> [Host]
    replaceHosts _ = parseHosts

    trigger :: [Host] -> [Host] -> IO ()
    trigger _ xs = do
      Prometheus.withLabel
        dnsDiscoveryCounter
        promLabels
        Prometheus.incCounter
      Prometheus.withLabel
        dnsDiscoveryGauge
        promLabels
        (flip Prometheus.setGauge (fromIntegral $ length xs))

    promLabels :: Label3
    promLabels = ("dig", Text.pack typ, Text.pack target)

    parseHosts :: ByteString -> [Host]
    parseHosts = Text.lines . Text.decodeUtf8

    tenSecs :: Maybe (MicroSeconds Int)
    tenSecs = Just 10_000_000

{-# NOINLINE dnsDiscoveryGauge #-}
dnsDiscoveryGauge :: Vector Label3 Gauge
dnsDiscoveryGauge =
  Prometheus.unsafeRegister
    $ Prometheus.vector ("m", "type", "host")
    $ Prometheus.gauge (Prometheus.Info "prodapi_dns_discovery_results" "")

{-# NOINLINE dnsDiscoveryCounter #-}
dnsDiscoveryCounter :: Vector Label3 Counter
dnsDiscoveryCounter =
  Prometheus.unsafeRegister
    $ Prometheus.vector ("m", "type", "host")
    $ Prometheus.counter (Prometheus.Info "prodapi_dns_discoveries" "")

cmdOut
  :: forall a.  String -- program to run
  -> [String] -- arguments to the program to run
  -> ByteString -- input submitted to the program
  -> Maybe (MicroSeconds Int) -- delay between invocations
  -> a
  -> (a -> ByteString -> a)
  -> (a -> a -> IO ())
  -> IO (Discovery a)
cmdOut cmd args input ms st0 update trigger =
    Discovery <$> background st0 NotAsked run
  where
    run :: a -> IO (Result a, a)
    run st0 = do
      maybe (pure ()) threadDelay ms
      out <- runCmd 
      let st1 = update st0 out
      now <- getCurrentTime
      seq st1 $ trigger st0 st1
      pure $ (Found now st1, st1)

    runCmd :: IO ByteString
    runCmd = (\(_,x,_) -> x) <$> readProcessWithExitCode cmd args input
