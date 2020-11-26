{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger as RequestLogger

import Data.Proxy (Proxy(..))
import Servant
import Servant.Server
import Prod.App as Prod
import qualified Prod.UserAuth as Auth

import qualified Hello
import qualified Monitors

type FullApi = Hello.Api
  :<|> Monitors.Api
  :<|> Auth.UserAuthApi

main :: IO ()
main = do
  init <- initialize Prod.defaultRuntime
  authRt <- Auth.initRuntime "secret-value" "pg://"
  helloRt <- Hello.initRuntime
  monitorsRt <- Monitors.initRuntime
  Warp.run
    8000
    $ RequestLogger.logStdoutDev
    $ appWithContext
        init
        (Hello.serve helloRt
         :<|> Monitors.handle monitorsRt
         :<|> Auth.handleUserAuth authRt)
        (Proxy @FullApi)
        (Auth.authServerContext authRt)
