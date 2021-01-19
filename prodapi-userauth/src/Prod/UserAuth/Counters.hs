{-# LANGUAGE StrictData #-}

module Prod.UserAuth.Counters
  ( Counters (..),
    initCounters,
  )
where

import Data.Text (Text)
import qualified Prometheus as Prometheus

data Counters
  = Counters
      { registrations :: Prometheus.Vector (Text) Prometheus.Counter,
        logins :: Prometheus.Vector (Text) Prometheus.Counter,
        echoes :: Prometheus.Vector (Text) Prometheus.Counter,
        whoamis :: Prometheus.Vector (Text) Prometheus.Counter,
        recoveryRequests :: Prometheus.Vector (Text) Prometheus.Counter,
        recoveryApplied :: Prometheus.Vector (Text) Prometheus.Counter
      }

initCounters :: IO Counters
initCounters =
  Counters
    <$> statuses "userauth_registrations" "number of registrations"
    <*> statuses "userauth_logins" "number of logins recorded"
    <*> statuses "userauth_echoes" "number of token-echo recorded"
    <*> statuses "userauth_whoamis" "number of whoamis received"
    <*> statuses "userauth_recovery_requests" "number of recovery-requests received"
    <*> statuses "userauth_recovery_applied" "number of recovery-requests applied"
  where
    statuses k h =
      Prometheus.register
        $ Prometheus.vector "status"
        $ Prometheus.counter (Prometheus.Info k h)
