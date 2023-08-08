{-# LANGUAGE TupleSections #-}

-- | Set of helpers to configure WarpTLS settings.
module Prod.Proxy.MultiApp where

import qualified Data.ByteString.Char8 as ByteString
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Network.Wai as WAI
import Network.Wai.Internal as WAI
import Network.Wai.Handler.WarpTLS as WarpTLS
import Network.TLS as TLS

-- TLS

type CredentialMap = Map TLS.HostName TLS.Credentials

type DefaultCredentials = TLS.Credentials

type X509Path = FilePath

type PrivateKeyPath = FilePath

loadCredentialMap :: [(HostName,X509Path,PrivateKeyPath)] -> IO (Either String CredentialMap)
loadCredentialMap configs = do
    loads <- traverse loadHostConfig configs
    pure $ fmap bundleMap (sequence loads)
  where
    loadHostConfig :: (HostName,X509Path,PrivateKeyPath) -> IO (Either String (HostName, Credential))
    loadHostConfig (h,x,k) = do
      c <- TLS.credentialLoadX509 x k
      pure $ fmap (h,) c
    bundleMap :: [(HostName, Credential)] -> CredentialMap
    bundleMap pairs = Map.fromListWith (<>) [(h, Credentials [c]) | (h,c) <- pairs]

type SNIHandler = Maybe HostName -> IO (Credentials)

toSNIHandler
  :: CredentialMap
  -> DefaultCredentials
  -> SNIHandler
toSNIHandler m def = \h -> case h of
  Nothing -> pure def
  Just x -> pure $ fromMaybe def $ Map.lookup x m

withTLSCredentialMap
  :: CredentialMap
  -> DefaultCredentials
  -> TLSSettings -> TLSSettings
withTLSCredentialMap m c tlsSetts =
  let
     hooks = (tlsServerHooks tlsSetts) { onServerNameIndication = toSNIHandler m c }
  in
  tlsSetts { tlsServerHooks = hooks }

-- WEB

type ApplicationMap = Map TLS.HostName WAI.Application

routeApplication :: ApplicationMap -> WAI.Application -> WAI.Application
routeApplication apps defaultApp = \req reply -> do
  let host = ByteString.unpack <$> WAI.requestHeaderHost req
  let routedApp = host >>= flip Map.lookup apps
  let app = fromMaybe defaultApp routedApp
  app req reply
