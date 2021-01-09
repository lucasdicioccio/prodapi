{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Monitors.Api where

import Servant
import Monitors.Base
import Prod.UserAuth.Api (CookieProtect)

type Api = "monitors" :>
  ( AddPing :<|> ReadPing :<|> RemovePing )

type AddPing = "ping"
  :> CookieProtect
  :> ReqBody '[JSON] PingTarget 
  :> Post '[JSON] Registration

type ReadPing = "ping"
  :> "latest"
  :> CookieProtect
  :> QueryParams "target" Registration 
  :> Get '[JSON] [Maybe CommandStatus]

type RemovePing = "ping"
  :> CookieProtect
  :> ReqBody '[JSON] Registration
  :> Delete '[JSON] DeRegistration
