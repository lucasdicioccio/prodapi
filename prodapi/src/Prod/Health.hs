{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Prod.Health
  ( HealthApi,
    handleHealth,
    Liveness (..),
    Reason (..),
    Readiness (..),
    Runtime (..),
    defaultRuntime,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), Value (String))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prod.Background (BackgroundVal, readBackgroundVal)
import Servant

data Runtime
  = Runtime
      { liveness :: IO Liveness,
        readiness :: IO Readiness
      }

defaultRuntime :: Runtime
defaultRuntime = Runtime (pure Alive) (pure Ready)

data Liveness = Alive

instance ToJSON Liveness where
  toJSON = const $ String "alive"

newtype Reason = Reason Text
  deriving
    (ToJSON)
    via Text

data Readiness = Ready | Ill [Reason]
  deriving (Generic)

instance ToJSON Readiness

type LivenessApi =
  Summary "Health liveness probe."
    :> "health"
    :> "alive"
    :> Get '[JSON] Liveness

type ReadinessApi =
  Summary "Health readiness probe."
    :> "health"
    :> "ready"
    :> Get '[JSON] Readiness

type HealthApi =
  LivenessApi
    :<|> ReadinessApi

handleHealth :: Runtime -> Server HealthApi
handleHealth runtime =
  handleLiveness runtime
    :<|> handleReadiness runtime
  where
    handleLiveness :: Runtime -> Handler Liveness
    handleLiveness = liftIO . liveness
    handleReadiness :: Runtime -> Handler Readiness
    handleReadiness = liftIO . readiness
