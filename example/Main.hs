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

type FullApi = Hello.Api :<|> Auth.UserAuthApi

main :: IO ()
main = do
  init <- initialize Prod.defaultRuntime
  authRt <- Auth.initRuntime "secret-value" "pg://"
  helloRt <- Hello.initRuntime
  Warp.run
    8000
    $ RequestLogger.logStdoutDev
    $ appWithContext
        init
        (Hello.serve helloRt :<|> Auth.handleUserAuth authRt)
        (Proxy @FullApi)
        (Auth.authServerContext authRt)
