{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Monitors.Api where

import Servant
import Monitors.Base

type Api = "monitors" :>
  ( AddPing :<|> RemovePing )

type AddPing = "ping"
  :> ReqBody '[JSON] PingTarget 
  :> Post '[JSON] Registration

type RemovePing = "ping"
  :> ReqBody '[JSON] Registration
  :> Delete '[JSON] DeRegistration
