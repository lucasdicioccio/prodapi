{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Monitors.Base where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Prod.Background
import Data.IORef (IORef, newIORef)

import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)
import System.Exit (ExitCode)

type InternetDestination = Text

newtype PingTarget = PingTarget InternetDestination
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type Registration = Text
type DeRegistration = Int

data Runtime = Runtime {
    pings :: IORef [(Registration, BackgroundVal (Maybe CommandOutput))]
  }

initRuntime :: IO Runtime
initRuntime = Runtime <$> newIORef []

type CommandOutput = (ExitCode, ByteString, ByteString)

backgroundPings :: PingTarget -> IO (BackgroundVal (Maybe CommandOutput))
backgroundPings (PingTarget tgt) =
  background
    ()
    Nothing
    go
    1000000
  where
    cmd = proc "ping"
          [ "-c", "3"
          , "-W", "1000"
          , Text.unpack tgt
          ]
    go _ =  do
      ret <- readCreateProcessWithExitCode cmd ""
      pure (Just ret, ())
