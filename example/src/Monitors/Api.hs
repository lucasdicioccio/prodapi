{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Monitors.Api where

import Servant
import Monitors.Base
import Prod.UserAuth.Api (CookieProtect)

type Api = "monitors" :>
  ( AddPing :<|> RemovePing )

type AddPing = "ping"
  :> CookieProtect
  :> ReqBody '[JSON] PingTarget 
  :> Post '[JSON] Registration

type RemovePing = "ping"
  :> CookieProtect
  :> ReqBody '[JSON] Registration
  :> Delete '[JSON] DeRegistration
