module Prod.Proxy.Compat where

import qualified Data.Text.Encoding as Text

import qualified Prod.Healthcheck as Healthcheck
import qualified Prometheus as Prometheus
import Prod.Proxy.Base
import Prod.Proxy.Lookups

-- | Generate a list of healthy backends from an health-checked result.
pickHealthy :: Healthcheck.SummaryMap -> [(Host,Port)]
pickHealthy =
    fmap adapt . Healthcheck.healthyKeys
  where
    adapt (hcHost, port) = (Text.encodeUtf8 hcHost, port)

-- | Count successful lookups.
countWith :: Prometheus.Counter -> LookupHostPort -> LookupHostPort
countWith cnt l1 = decorateSuccessWith (const $ Prometheus.incCounter cnt) l1

