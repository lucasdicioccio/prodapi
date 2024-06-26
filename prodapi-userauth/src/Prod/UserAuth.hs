{-# LANGUAGE OverloadedRecordDot #-}

module Prod.UserAuth (
    UserAuthApi,
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
    renderStatus,
    --
    Track (..),
    BehaviourTrack (..),
    BackendTrack (..),
    JwtTrack (..),
    CallbackTrack (..),
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
import Prod.UserAuth.Runtime (Counters (..), Runtime, counters, initRuntime, secretstring, tokenValidityDuration, traceAttempt, traceAugmentCookie, traceRecoveryApplication, traceRecoveryRequest, traceRegistration, withConn)
import Prod.UserAuth.Trace
import qualified Prometheus as Prometheus
import Servant
import Servant.Server

import Lucid

handleUserAuth :: Runtime info -> Server (UserAuthApi info)
handleUserAuth runtime =
    handleEchoCookieClaims runtime
        :<|> handleWhoAmI runtime
        :<|> handleRenewCookie runtime
        :<|> handleCleanCookie
        :<|> handleRegister runtime
        :<|> handleLogin runtime
        :<|> handleRecoveryRequest runtime
        :<|> handleApplyRecovery runtime
        :<|> handleHello

handleEchoCookieClaims :: Runtime info -> Maybe LoggedInCookie -> Handler JWTClaimsSet
handleEchoCookieClaims runtime cookie = do
    inc echoes "echoed" (counters runtime)
    withLoginCookieVerified runtime cookie (pure . claims)

handleRenewCookie ::
    Runtime info ->
    Maybe LoggedInCookie ->
    Handler (Headers '[Header "Set-Cookie" LoggedInCookie] Text)
handleRenewCookie runtime cookie = do
    inc echoes "renewed" (counters runtime)
    withLoginCookieVerified runtime cookie $ \jwt -> do
        let muid = Map.lookup "user-id" (unClaimsMap . unregisteredClaims $ claims jwt)
        case muid of
            Just (Number nid) -> do
                inc renewals "valid" (counters runtime)
                let uid = truncate nid :: UserId
                cookie <- liftIO $ makeLoggedInCookie runtime uid
                liftIO $ traceAugmentCookie runtime cookie
                case cookie of
                    Left _ -> do
                        inc renewals "renew-failed" (counters runtime)
                        throwError $ err500{errBody = "failed to renew cookie"}
                    Right c -> do
                        inc renewals "renew-ok" (counters runtime)
                        pure $ addHeader c $ encodedJwt c
            _ -> do
                inc renewals "token-invalid" (counters runtime)
                throwError $ err400{errBody = "this cookie is wrong"}

handleWhoAmI :: Runtime info -> Maybe LoggedInCookie -> Handler [WhoAmI info]
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
                throwError $ err400{errBody = "this cookie is wrong"}

handleHello :: UserAuthInfo -> Handler Text
handleHello (UserAuthInfo jwt) = do
    pure "hello world"

handleCleanCookie ::
    Handler (Headers '[Header "Set-Cookie" LoggedInCookie] ())
handleCleanCookie =
    pure $
        addHeader (LoggedInCookie "logged-off") ()

handleRegister ::
    Runtime info ->
    RegistrationRequest ->
    Handler (Headers '[Header "Set-Cookie" LoggedInCookie] (RegistrationResult info))
handleRegister runtime req = do
    inc registrations "requested" (counters runtime)
    res <- liftIO $ registerIO runtime req
    traceRegistration runtime req res
    withheaders <- liftIO $ wrapHeader res
    inc registrations "ok" (counters runtime)
    pure $ withheaders res
  where
    wrapHeader :: RegistrationResult info -> IO (a -> Headers '[Header "Set-Cookie" LoggedInCookie] a)
    wrapHeader res = case res of
        RegisterFailure -> pure noHeader
        RegisterSuccess dat -> do
            cookie <- makeLoggedInCookie runtime (userId dat)
            traceAugmentCookie runtime cookie
            case cookie of
                Left _ -> pure noHeader
                Right c -> pure $ addHeader c

handleLogin ::
    Runtime info ->
    LoginAttempt ->
    Handler (Headers '[Header "Set-Cookie" LoggedInCookie] (LoginResult info))
handleLogin runtime attempt = do
    inc logins "requested" (counters runtime)
    res <- liftIO $ loginIO runtime attempt
    traceAttempt runtime attempt res
    withheaders <- liftIO $ wrapHeader res
    pure $ withheaders res
  where
    wrapHeader :: LoginResult info -> IO (a -> Headers '[Header "Set-Cookie" LoggedInCookie] a)
    wrapHeader res = case res of
        LoginFailed -> do
            inc logins "ko" (counters runtime)
            pure noHeader
        LoginSuccess dat -> do
            inc logins "ok" (counters runtime)
            cookie <- makeLoggedInCookie runtime (userId dat)
            traceAugmentCookie runtime cookie
            case cookie of
                Left _ -> pure noHeader
                Right c -> pure $ addHeader c

handleRecoveryRequest :: Runtime info -> RecoveryRequest -> Handler RecoveryRequestNotification
handleRecoveryRequest runtime req = do
    inc recoveryRequests "requested" (counters runtime)
    xs <- liftIO $ recoveryRequestIO runtime req
    let token = case xs of [] -> ""; (x : _) -> tokenValue x :: Text
    inc recoveryRequests "ok" (counters runtime)
    let res = RecoveryRequestNotification (req.email) tokenValidityDuration token
    traceRecoveryRequest runtime req res
    pure res

handleApplyRecovery :: Runtime info -> ApplyRecoveryRequest -> Handler RecoveryResult
handleApplyRecovery runtime req = do
    inc recoveryApplied "requested" (counters runtime)
    res <- liftIO $ applyRecoveryIO runtime req
    traceRecoveryApplication runtime req res
    case res of
        RecoverySuccess -> pure res <* inc recoveryApplied "ok" (counters runtime)
        _ -> throwError err403

inc ::
    (MonadIO m) =>
    (a -> Prometheus.Vector (Text) Prometheus.Counter) ->
    Text ->
    a ->
    m ()
inc f s cnts =
    liftIO $ Prometheus.withLabel (f cnts) s Prometheus.incCounter

renderStatus :: RenderStatus a
renderStatus = const $ section_ $ do
    h1_ "user-auth"
    h4_ "login"
    p_ $ with form_ [id_ "login-form", action_ "/user-auth/login", method_ "post"] $ do
        p_ $ do
            label_ [for_ "login-email", type_ "text"] "email"
            input_ [type_ "text", id_ "login-email", name_ "email"]
        p_ $ do
            label_ [for_ "login-plain", type_ "text"] "password"
            input_ [type_ "password", id_ "login-plain", name_ "plain"]
        p_ $ do
            input_ [type_ "submit", value_ "login"]
    h4_ "register"
    p_ $ with form_ [id_ "register-form", action_ "/user-auth/registration", method_ "post"] $ do
        p_ $ do
            label_ [for_ "register-email", type_ "text"] "email"
            input_ [type_ "text", id_ "register-email", name_ "email"]
        p_ $ do
            label_ [for_ "register-plain", type_ "text"] "password"
            input_ [type_ "password", id_ "register-plain", name_ "plain"]
        p_ $ do
            input_ [type_ "submit", value_ "register"]
    h4_ "recovery-request"
    p_ $ with form_ [id_ "recovery-request-form", action_ "/user-auth/recovery/request", method_ "post"] $ do
        p_ $ do
            label_ [for_ "recovery-request-email", type_ "text"] "email"
            input_ [type_ "text", id_ "recovery-request-email", name_ "email"]
        p_ $ do
            input_ [type_ "submit", value_ "recover"]
    h4_ "recovery-apply"
    p_ $ with form_ [id_ "recovery-apply-form", action_ "/user-auth/recovery/apply", method_ "post"] $ do
        p_ $ do
            label_ [for_ "recovery-apply-email", type_ "text"] "email"
            input_ [type_ "text", id_ "recovery-apply-email", name_ "email"]
        p_ $ do
            label_ [for_ "recovery-apply-plain", type_ "text"] "new-password"
            input_ [type_ "password", id_ "recovery-apply-plain", name_ "plain"]
        p_ $ do
            label_ [for_ "recovery-apply-token", type_ "text"] "token"
            input_ [type_ "password", id_ "recovery-apply-token", name_ "token"]
        p_ $ do
            input_ [type_ "submit", value_ "recover"]
    h4_ "misc"
    p_ $ with a_ [href_ "/user-auth/whoami"] "whoami"
    p_ $ with a_ [href_ "/user-auth/echo-cookie"] "echo-cookie"
    p_ $
        with form_ [action_ "/user-auth/clean-cookie", method_ "post"] $
            input_ [type_ "submit", value_ "clear cookie"]
