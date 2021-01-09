{-# LANGUAGE DuplicateRecordFields #-}

module Prod.UserAuth
  ( UserAuthApi,
    handleUserAuth,
    initRuntime,
    Runtime,
    --
    withLoginCookieVerified,
    withOptionalLoginCookieVerified,
    --
    CookieProtect,
    UserAuthInfo (..),
    authUserId,
    Request,
    authServerContext,
    --
    authorized,
    limited,
    --
    renderStatus
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (Number))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Prod.Status (RenderStatus)
import Prod.UserAuth.Api (CookieProtect, UserAuthApi)
import Prod.UserAuth.Backend
import Prod.UserAuth.Base
import Prod.UserAuth.HandlerCombinators
import Prod.UserAuth.JWT
import Prod.UserAuth.Runtime (Counters (..), Runtime, counters, initRuntime, secretstring, tokenValidityDuration, withConn)
import qualified Prometheus as Prometheus
import Servant
import Servant.Server

import Lucid

handleUserAuth :: Runtime -> Server UserAuthApi
handleUserAuth runtime =
  handleEchoCookieClaims runtime
    :<|> handleWhoAmI runtime
    :<|> handleCleanCookie
    :<|> handleRegister runtime
    :<|> handleLogin runtime
    :<|> handleRecoveryRequest runtime
    :<|> handleApplyRecovery runtime
    :<|> handleHello

handleEchoCookieClaims :: Runtime -> Maybe LoggedInCookie -> Handler JWTClaimsSet
handleEchoCookieClaims runtime cookie = do
  inc echoes "requested" (counters runtime)
  withLoginCookieVerified runtime cookie (pure . claims)

handleWhoAmI :: Runtime -> Maybe LoggedInCookie -> Handler [WhoAmI]
handleWhoAmI runtime cookie = do
  inc whoamis "requested" (counters runtime)
  withLoginCookieVerified runtime cookie $ \jwt -> do
    let muid = Map.lookup "user-id" (unClaimsMap . unregisteredClaims $ claims jwt)
    case muid of
      Just (Number nid) -> do
        inc whoamis "success" (counters runtime)
        let uid = truncate nid :: UserId
        liftIO $ whoAmIQueryIO runtime uid
      _ -> do
        inc whoamis "token-corrupted" (counters runtime)
        throwError $ err400 {errBody = "this cookie is wrong"}

handleHello :: UserAuthInfo -> Handler Text
handleHello (UserAuthInfo jwt) = do
  pure "hello world"

handleCleanCookie ::
  Handler (Headers '[Header "Set-Cookie" LoggedInCookie] ())
handleCleanCookie =
  pure $
    addHeader (LoggedInCookie "logged-off") ()

handleRegister ::
  Runtime ->
  RegistrationRequest ->
  Handler (Headers '[Header "Set-Cookie" LoggedInCookie] RegistrationResult)
handleRegister runtime req = do
  inc registrations "requested" (counters runtime)
  res <- liftIO $ registerIO runtime req
  withheaders <- liftIO $ wrapHeader res
  inc registrations "ok" (counters runtime)
  pure $ withheaders res
  where
    wrapHeader :: RegistrationResult -> IO (a -> Headers '[Header "Set-Cookie" LoggedInCookie] a)
    wrapHeader res = case res of
      RegisterFailure -> pure noHeader
      RegisterSuccess dat -> addHeader <$> makeLoggedInCookie runtime (userId dat)

handleLogin ::
  Runtime ->
  LoginAttempt ->
  Handler (Headers '[Header "Set-Cookie" LoggedInCookie] LoginResult)
handleLogin runtime attempt = do
  inc logins "requested" (counters runtime)
  res <- liftIO $ loginIO runtime attempt
  withheaders <- liftIO $ wrapHeader res
  pure $ withheaders res
  where
    wrapHeader :: LoginResult -> IO (a -> Headers '[Header "Set-Cookie" LoggedInCookie] a)
    wrapHeader res = case res of
      LoginFailed -> do
        inc logins "ko" (counters runtime)
        pure noHeader
      LoginSuccess dat -> do
        inc logins "ok" (counters runtime)
        addHeader <$> makeLoggedInCookie runtime (userId dat)

handleRecoveryRequest :: Runtime -> RecoveryRequest -> Handler RecoveryRequestNotification
handleRecoveryRequest runtime req = do
  inc recoveryRequests "requested" (counters runtime)
  _ <- liftIO $ recoveryRequestIO runtime req
  inc recoveryRequests "ok" (counters runtime)
  pure $ RecoveryRequestNotification (email (req :: RecoveryRequest)) tokenValidityDuration

handleApplyRecovery :: Runtime -> ApplyRecoveryRequest -> Handler RecoveryResult
handleApplyRecovery runtime req = do
  inc recoveryApplied "requested" (counters runtime)
  res <- liftIO $ applyRecoveryIO runtime req
  case res of
    RecoverySuccess -> pure res <* inc recoveryApplied "ok" (counters runtime)
    _ -> throwError err403

inc ::
  MonadIO m =>
  (a -> Prometheus.Vector (Text) Prometheus.Counter) ->
  Text ->
  a ->
  m ()
inc f s cnts =
  liftIO $ Prometheus.withLabel (f cnts) s Prometheus.incCounter

renderStatus :: RenderStatus a
renderStatus = const $ section_ $ do
  h1_ "user-auth"
  with a_ [ href_ "/user-auth/whoami" ] "whoami"
