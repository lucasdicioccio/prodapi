-- TODO:
-- - more counters (on failure)
-- - tracer

module Prod.Proxy.Lookups where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import System.Random.Shuffle (shuffleM)
import Prod.Health (Readiness(..))
import qualified Prod.Healthcheck as Healthcheck

import Prod.Proxy.Base

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

-- backend compositions

firstHealthy :: Healthcheck.Runtime -> LookupHostPort
firstHealthy rt =
    randomBackend (fmap adapt . Healthcheck.healthy <$> Healthcheck.readBackgroundChecks rt)
  where
    adapt (hcHost, port) = (Text.encodeUtf8 hcHost, port)

firstBackend :: IO [(Host, Port)] -> LookupHostPort
firstBackend disc = const f
  where
    f :: IO (Maybe (Host, Port))
    f = safeHead <$> disc

randomBackend :: IO [(Host,Port)] -> LookupHostPort
randomBackend disc = const f
  where
    f :: IO (Maybe (Host, Port))
    f = safeHead <$> (disc >>= shuffleM)

fallback :: LookupHostPort -> LookupHostPort -> LookupHostPort
fallback l1 l2 = \req -> do
  o1 <- l1 req
  case o1 of
    Nothing -> l2 req
    Just _ -> pure o1

