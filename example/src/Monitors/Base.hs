{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Monitors.Base where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import GHC.Generics (Generic)
import Prod.Background
import Data.IORef (IORef, newIORef)

import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)
import System.Exit (ExitCode(..))

import Monitors.Counters (Counters(..), newCounters)
import qualified Prometheus as Prometheus

type InternetDestination = Text

newtype PingTarget = PingTarget InternetDestination
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type Registration = Text
type DeRegistration = Int

data Runtime = Runtime {
    pings :: IORef [(Registration, BackgroundVal (Maybe CommandOutput))]
  , counters :: Counters
  }

initRuntime :: IO Runtime
initRuntime = Runtime <$> newIORef [] <*> newCounters

type CommandOutput = (ExitCode, ByteString, ByteString)

backgroundPings
  :: Counters
  -> PingTarget
  -> IO (BackgroundVal (Maybe CommandOutput))
backgroundPings counters (PingTarget tgt) =
  backgroundLoop
    Nothing
    go
    1000000
  where
    cmd = proc "ping"
          [ "-c", "3"
          , "-W", "1000"
          , Text.unpack tgt
          ]
    labelForExitCode ExitSuccess = "0"
    labelForExitCode (ExitFailure n) = Text.pack $ show $ n
    go =  do
      Prometheus.withLabel (executionStarted counters) "ping" Prometheus.incCounter
      ret@(code,out,_) <- readCreateProcessWithExitCode cmd ""
      Prometheus.withLabel (executionEnded counters) ("ping", labelForExitCode code) Prometheus.incCounter
      let stdoutRows = length $ ByteString.lines out
      Prometheus.withLabel (stdoutLines counters) "ping" $ \cnt -> void $ do
        Prometheus.addCounter cnt (fromIntegral stdoutRows)
      pure $ Just ret
