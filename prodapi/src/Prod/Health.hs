{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Prod.Health
  ( HealthApi,
    handleHealth,
    Liveness (..),
    Reason (..),
    Readiness (..),
    completeReadiness,
    Runtime (..),
    alwaysReadyRuntime,
    withLiveness,
    withReadiness,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), Value (String))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

data Runtime
  = Runtime
      { liveness :: IO Liveness,
        readiness :: IO Readiness,
        conditions :: IORef (Set Reason)
      }

alwaysReadyRuntime :: IO Runtime
alwaysReadyRuntime = Runtime (pure Alive) (pure Ready) <$> (newIORef mempty)

withReadiness :: IO Readiness -> Runtime -> Runtime
withReadiness io rt = rt { readiness = io }

withLiveness :: IO Liveness -> Runtime -> Runtime
withLiveness io rt = rt { liveness = io }

data Liveness = Alive

instance ToJSON Liveness where
  toJSON = const $ String "alive"

newtype Reason = Reason Text
  deriving stock (Eq, Ord)
  deriving
    (ToJSON)
    via Text

data Readiness = Ready | Ill (Set Reason)
  deriving (Generic)

instance ToJSON Readiness

combineReasons :: Readiness -> Set Reason -> Readiness
combineReasons Ready rs 
  | Set.null rs = Ready
  | otherwise   = Ill rs
combineReasons (Ill rs1) rs2 = Ill (rs1 <> rs2)

completeReadiness :: Runtime -> IO Readiness
completeReadiness rt =
 combineReasons <$> readiness rt <*> readIORef (conditions rt)

-- | Add some illness reason.
afflict :: Runtime -> Reason -> IO ()
afflict rt r =
  atomicModifyIORef' (conditions rt) (\rs -> (Set.insert r rs, ()))

-- | Remove some illness reason.
cure :: Runtime -> Reason -> IO ()
cure rt r = undefined
  atomicModifyIORef' (conditions rt) (\rs -> (Set.delete r rs, ()))

type GetLivenessApi =
  Summary "Health liveness probe."
    :> "health"
    :> "alive"
    :> Get '[JSON] Liveness

type GetReadinessApi =
  Summary "Health readiness probe."
    :> "health"
    :> "ready"
    :> Get '[JSON] Readiness

type DrainApi =
  Summary "Set a specific 'drained' condition."
    :> "health"
    :> "drain"
    :> Post '[JSON] Readiness

type HealthApi =
  GetLivenessApi
    :<|> GetReadinessApi
    :<|> DrainApi

handleHealth :: Runtime -> Server HealthApi
handleHealth runtime =
  handleLiveness runtime
    :<|> handleReadiness runtime
    :<|> handleDrain runtime
  where
    handleLiveness :: Runtime -> Handler Liveness
    handleLiveness = liftIO . liveness
    handleReadiness :: Runtime -> Handler Readiness
    handleReadiness rt = liftIO $ do
      completeReadiness rt
    handleDrain :: Runtime -> Handler Readiness
    handleDrain rt = do
      liftIO $ afflict rt (Reason "drained")
      handleReadiness rt
