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
import Prod.Background
import qualified Prod.UserAuth as Auth

import Data.Text (Text)

type HelloApi = "hello-world" :> Get '[JSON] Text

type FullApi = HelloApi :<|> Auth.UserAuthApi

handleHelloApi :: Handler Text
handleHelloApi = pure "hello world"

main :: IO ()
main = do
  init <- initialize Prod.defaultRuntime
  authRt <- Auth.initRuntime "secret-value" "pg://"
  Warp.run
    8000
    $ RequestLogger.logStdoutDev
    $ appWithContext
        init
        (handleHelloApi :<|> Auth.handleUserAuth authRt)
        (Proxy @FullApi)
        (Auth.authServerContext authRt)
