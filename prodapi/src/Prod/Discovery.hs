{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for performing service (endpoints) discovery.
module Prod.Discovery where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, getCurrentTime)

import Prod.Background (BackgroundVal, MicroSeconds, background, readBackgroundVal)
import qualified Prod.Background
import Prod.Tracer (Tracer (..), contramap)
import Prometheus (Counter, Gauge, Label3, Vector)
import qualified Prometheus as Prometheus
import System.Process.ByteString (readProcessWithExitCode)

data Track a = BackgroundTrack (Prod.Background.Track (Result a))
    deriving (Show, Functor)

data Result a
    = NotAsked
    | Asked UTCTime
    | Found UTCTime a
    deriving (Show, Functor)

toMaybe :: Result a -> Maybe a
toMaybe (Found _ a) = Just a
toMaybe _ = Nothing

data Discovery a = Discovery (BackgroundVal (Result a))
    deriving (Functor)

readCurrent :: Discovery a -> IO (Result a)
readCurrent (Discovery b) = readBackgroundVal b

type Host = Text

data DNSTrack a = DNSTrack Text Host (Track a)
    deriving (Show, Functor)

dnsA :: Tracer IO (DNSTrack [Host]) -> Host -> IO (Discovery [Host])
dnsA tracer hostname = dig (contramap (DNSTrack hostname "A") tracer) "A" $ Text.unpack hostname

dnsAAAA :: Tracer IO (DNSTrack [Host]) -> Host -> IO (Discovery [Host])
dnsAAAA tracer hostname = dig (contramap (DNSTrack hostname "AAAA") tracer) "AAAA" $ Text.unpack hostname

dig :: Tracer IO (Track [Host]) -> String -> String -> IO (Discovery [Host])
dig tracer typ target = cmdOut tracer "dig" ["+short", typ, target] "" tenSecs [] replaceHosts trigger
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
    Prometheus.unsafeRegister $
        Prometheus.vector ("m", "type", "host") $
            Prometheus.gauge (Prometheus.Info "prodapi_dns_discovery_results" "")

{-# NOINLINE dnsDiscoveryCounter #-}
dnsDiscoveryCounter :: Vector Label3 Counter
dnsDiscoveryCounter =
    Prometheus.unsafeRegister $
        Prometheus.vector ("m", "type", "host") $
            Prometheus.counter (Prometheus.Info "prodapi_dns_discoveries" "")

cmdOut ::
    forall a.
    Tracer IO (Track a) ->
    String -> -- program to run
    [String] -> -- arguments to the program to run
    ByteString -> -- input submitted to the program
    Maybe (MicroSeconds Int) -> -- delay between invocations
    a ->
    (a -> ByteString -> a) ->
    (a -> a -> IO ()) ->
    IO (Discovery a)
cmdOut tracer cmd args input ms st0 update trigger =
    Discovery <$> background (contramap BackgroundTrack tracer) st0 NotAsked run
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
    runCmd = (\(_, x, _) -> x) <$> readProcessWithExitCode cmd args input
