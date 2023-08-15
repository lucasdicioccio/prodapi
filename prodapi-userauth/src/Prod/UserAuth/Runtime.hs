{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Prod.UserAuth.Runtime
  ( Counters (..),
    initRuntime,
    Runtime,
    AugmentSession,
    augmentSession,
    AugmentWhoAmI,
    augmentWhoAmI,
    AugmentWhoAmI,
    augmentLoggedInCookieClaims,
    AugmentCookieClaims,
    counters,
    secretstring,
    baseClaimsSet,
    withConn,
    Connection,
    tokenValidityDuration,
    trace,
    traceTransaction,
    traceAttempt,
    traceVerification,
    traceOptionalVerification,
    traceRegistration,
    traceRecoveryRequest,
    traceRecoveryApplication,
    traceAllowed,
    traceDisallowed,
    traceLimited,
    traceJWT,
    traceJWTSigned,
    traceAugmentCookie,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, execute, query, rollback, withTransaction)
import Prod.UserAuth.Base (ErrorMessage, Minutes, SessionData, WhoAmI, UserId, LoginAttempt, LoginResult, RegistrationRequest, RegistrationResult, RecoveryRequest, RecoveryRequestNotification, ApplyRecoveryRequest, RecoveryResult, LoggedInCookie)
import Prod.UserAuth.Counters (Counters (..), initCounters)
import Prod.UserAuth.Trace (Track(..), BackendTrack(..), BehaviourTrack(..), JwtTrack(..), CallbackTrack(..))
import Prod.Tracer (Tracer(..))
import Web.JWT

data Runtime info
  = Runtime
      { counters :: !Counters,
        secretstring :: !Text,
        baseClaimsSet :: !JWTClaimsSet,
        connstring :: !ByteString,
        augmentSession :: AugmentSession info,
        augmentWhoAmI :: AugmentWhoAmI info,
        augmentLoggedInCookieClaims :: AugmentCookieClaims,
        tracer :: Tracer IO (Track info)
      }

type AugmentSession info = Connection -> SessionData () -> IO (Maybe info)

type AugmentWhoAmI info = Connection -> WhoAmI UserId -> IO (Maybe info)

type AugmentCookieClaims = UserId -> IO (Either ErrorMessage [(Text, Value)])

trace :: MonadIO m => Runtime a -> Track a -> m ()
trace rt v = liftIO $ runTracer (tracer rt) $ v

initRuntime
  :: Text
  -> JWTClaimsSet
  -> ByteString
  -> AugmentSession info
  -> AugmentWhoAmI info
  -> AugmentCookieClaims
  -> Tracer IO (Track info)
  -> IO (Runtime info)
initRuntime skret claims cstring augSession augWhoAmI augCookie tracer =
  Runtime
    <$> initCounters
    <*> pure skret
    <*> pure claims
    <*> pure cstring
    <*> pure augSession
    <*> pure augWhoAmI
    <*> pure augCookie
    <*> pure tracer

withConn :: Runtime b -> (Connection -> IO a) -> IO a
withConn runtime act = do
  conn <- connectPostgreSQL (connstring runtime)
  trace runtime (Backend SQLConnect)
  !ret <- act conn
  close conn
  pure ret

traceTransaction :: Runtime a -> Connection -> IO b -> IO b
traceTransaction runtime conn act =
  withTransaction conn $ do
    trace runtime (Backend SQLTransaction)
    act

traceAttempt :: MonadIO m => Runtime a -> LoginAttempt -> LoginResult a -> m ()
traceAttempt runtime l r =
  liftIO $ trace runtime $ Behaviour $ Attempt l r

traceVerification :: MonadIO m => Runtime a -> Bool -> m ()
traceVerification runtime bool =
  liftIO $ trace runtime $ Behaviour $ Verification bool

traceOptionalVerification :: MonadIO m => Runtime a -> Bool -> m ()
traceOptionalVerification runtime bool =
  liftIO $ trace runtime $ Behaviour $ OptionalVerification bool

traceRegistration :: MonadIO m => Runtime a -> RegistrationRequest -> RegistrationResult a -> m ()
traceRegistration runtime req res =
  liftIO $ trace runtime $ Behaviour $ Registration req res

traceRecoveryRequest :: MonadIO m => Runtime a -> RecoveryRequest -> RecoveryRequestNotification -> m ()
traceRecoveryRequest runtime req res =
  liftIO $ trace runtime $ Behaviour $ Recovery req res

traceRecoveryApplication :: MonadIO m => Runtime a -> ApplyRecoveryRequest -> RecoveryResult -> m ()
traceRecoveryApplication runtime req res =
  liftIO $ trace runtime $ Behaviour $ ApplyRecovery req res

traceAllowed,traceDisallowed,traceLimited :: MonadIO m => Runtime a -> m ()
traceAllowed runtime =
  liftIO $ trace runtime $ Bearer $ Allowed (Just True)
traceDisallowed runtime =
  liftIO $ trace runtime $ Bearer $ Allowed (Just False)
traceLimited runtime =
  liftIO $ trace runtime $ Bearer $ Allowed Nothing

traceJWT :: MonadIO m => (Runtime a) -> Maybe (JWT (VerifiedJWT)) -> m ()
traceJWT runtime jwt =
  liftIO $ trace runtime $ Bearer $ Extracted jwt

traceJWTSigned :: MonadIO m => (Runtime a) -> UserId -> [(Text,Value)] -> m ()
traceJWTSigned runtime uid claims =
  liftIO $ trace runtime $ Bearer $ Signed uid claims

traceAugmentCookie :: MonadIO m => (Runtime a) -> Either ErrorMessage LoggedInCookie -> m ()
traceAugmentCookie runtime c =
  liftIO $ trace runtime $ Callbacks $ AugmentCookie c

tokenValidityDuration :: Minutes
tokenValidityDuration = 30
