module Monitors.Handlers (handle) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', readIORef)
import qualified Data.List as List
import Monitors.Api
import Monitors.Base
import Monitors.Counters
import Prod.Background (BackgroundVal, kill, readBackgroundVal)
import Prod.UserAuth (UserAuthInfo, authorized)
import Servant
import Servant.Server

handle :: Runtime -> Server Api
handle rt = addPing rt :<|> readPing rt :<|> removePing rt :<|> removePing rt

addPing :: Runtime -> UserAuthInfo -> PingTarget -> Handler Registration
addPing rt auth tgt@(PingTarget target) = authorized (authRt rt) auth $ \id -> liftIO $ do
    bkg <- backgroundPings (counters rt) (tracer rt) tgt
    let reg = Registration target
    _ <- atomicModifyIORef' (pings rt) (\xs -> ((reg, bkg) : xs, ()))
    logRegistered rt reg
    pure $ reg

readPing :: Runtime -> UserAuthInfo -> [Registration] -> Handler [Maybe CommandStatus]
readPing rt auth tgts = authorized (authRt rt) auth $ \id -> liftIO $ do
    registrations <- readIORef (pings rt)
    traverse (lookupOne registrations) tgts
  where
    lookupOne :: [(Registration, BackgroundVal (Maybe CommandOutput))] -> Registration -> IO (Maybe CommandStatus)
    lookupOne xs reg = do
        let bval = lookup reg xs
        maybe (pure Nothing) ((fmap . fmap) outputToStatus . readBackgroundVal) bval

removePing :: Runtime -> UserAuthInfo -> Registration -> Handler DeRegistration
removePing rt auth regname = authorized (authRt rt) auth $ \id -> liftIO $ do
    let kept (r, _) = r /= regname
    bkgs <- atomicModifyIORef' (pings rt) (List.partition kept)
    traverse kill $ fmap snd $ bkgs
    pure $ length bkgs
