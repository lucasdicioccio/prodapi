{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Monitors.Api where

import Monitors.Base
import Prod.UserAuth.Api (CookieProtect)
import Servant

type Api =
    "monitors"
        :> (AddPing :<|> ReadPing :<|> RemovePing :<|> PostRemovePing)

type AddPing =
    "ping"
        :> CookieProtect
        :> ReqBody '[FormUrlEncoded, JSON] PingTarget
        :> Post '[JSON] Registration

type ReadPing =
    "ping"
        :> "latest"
        :> CookieProtect
        :> QueryParams "target" Registration
        :> Get '[JSON] [Maybe CommandStatus]

type RemovePing =
    "ping"
        :> CookieProtect
        :> ReqBody '[JSON] Registration
        :> Delete '[JSON] DeRegistration

type PostRemovePing =
    "ping-delete"
        :> CookieProtect
        :> ReqBody '[FormUrlEncoded] Registration
        :> Post '[JSON] DeRegistration
