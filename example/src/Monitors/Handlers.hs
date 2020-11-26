
module Monitors.Handlers (handle) where

import Servant
import Servant.Server
import Monitors.Api
import Monitors.Base
import Monitors.Counters
import Data.IORef (atomicModifyIORef')
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import Prod.Background (kill)

handle :: Runtime -> Server Api
handle rt = addPing rt :<|> removePing rt

addPing :: Runtime -> PingTarget -> Handler Registration
addPing rt tgt@(PingTarget target) = liftIO $ do
  bkg <- backgroundPings tgt
  let regname = target
  _ <- atomicModifyIORef' (pings rt) (\xs -> ((regname, bkg) : xs, ()))
  pure $ regname

removePing :: Runtime -> Registration -> Handler DeRegistration
removePing rt regname = liftIO $ do
  let kept (r, _) = r /= regname
  bkgs <- atomicModifyIORef' (pings rt) (List.partition kept)
  traverse kill $ fmap snd $ bkgs
  pure $ length bkgs
