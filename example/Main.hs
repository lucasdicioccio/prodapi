{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger as RequestLogger

import Data.Aeson (ToJSON(..))
import Data.Proxy (Proxy(..))
import Servant
import Servant.Server
import Prod.App as Prod
import Prod.Status (statusPage)
import qualified Prod.UserAuth as Auth

import qualified Hello
import qualified Monitors
import qualified Monitors.Base as Monitors

import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import Lucid (HtmlT, ToHtml(..), h4_, div_, p_, ul_, li_, a_, href_, with)

type FullApi = Hello.Api
  :<|> Monitors.Api
  :<|> Auth.UserAuthApi

data ExampleStatus = ExampleStatus { registrations :: [Monitors.Registration] }
  deriving (Generic)
instance ToJSON ExampleStatus
instance ToHtml ExampleStatus where
  toHtml = renderStatus
  toHtmlRaw = renderStatus

renderStatus :: forall m. (Monad m) => ExampleStatus -> HtmlT m ()
renderStatus (ExampleStatus regs) = div_ $ do
    h4_ "example status"
    p_ $ toHtml $ "registrations (" <> (show $ length regs) <> ")"
    ul_ $ do
      traverse_ (renderRegistration) regs
  where
    renderRegistration :: Monitors.Registration -> HtmlT m ()
    renderRegistration reg = li_ $
      with a_ [ href_ (readMonitorUrl reg) ] $ toHtml reg

    readMonitorUrl reg = "/monitors/ping/latest?target=" <> reg

exampleStatus :: Monitors.Runtime -> IO ExampleStatus
exampleStatus mRt = do
  ExampleStatus
    <$> Monitors.readRegistrations mRt

main :: IO ()
main = do
  healthRt <- Prod.alwaysReadyRuntime
  init <- initialize healthRt
  authRt <- Auth.initRuntime "secret-value" "postgres://prodapi:prodapi@localhost:5432/prodapi_example"
  helloRt <- Hello.initRuntime
  monitorsRt <- Monitors.initRuntime
  Warp.run
    8000
    $ RequestLogger.logStdoutDev
    $ appWithContext
        init
        (exampleStatus monitorsRt)
        (statusPage)
        (Hello.serve helloRt
         :<|> Monitors.handle monitorsRt
         :<|> Auth.handleUserAuth authRt)
        (Proxy @FullApi)
        (Auth.authServerContext authRt)
