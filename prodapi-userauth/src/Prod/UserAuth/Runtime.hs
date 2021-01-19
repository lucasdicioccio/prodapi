{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Prod.UserAuth.Runtime
  ( Counters (..),
    initRuntime,
    Runtime,
    counters,
    secretstring,
    withConn,
    Connection,
    tokenValidityDuration,
    trace,
    traceTransaction,
    traceAttempt,
    traceVerification,
    traceOptionalVerification,
    traceAllowed,
    traceDisallowed,
    traceLimited,
    traceJWT,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, execute, query, rollback, withTransaction)
import Prod.UserAuth.Base (Minutes, LoginAttempt)
import Prod.UserAuth.Counters (Counters (..), initCounters)
import Prod.UserAuth.Trace (Track(..), BackendTrack(..), BehaviourTrack(..), JwtTrack(..))
import Prod.Tracer (Tracer(..))
import Web.JWT

data Runtime
  = Runtime
      { counters :: !Counters,
        secretstring :: !Text,
        connstring :: !ByteString,
        tracer :: Tracer IO Track
      }

trace :: MonadIO m => Runtime -> Track -> m ()
trace rt v = liftIO $ runTracer (tracer rt) $ v

initRuntime :: Text -> ByteString -> Tracer IO Track -> IO Runtime
initRuntime skret cstring tracer =
  Runtime
    <$> initCounters
    <*> pure skret
    <*> pure cstring
    <*> pure tracer

withConn :: Runtime -> (Connection -> IO a) -> IO a
withConn runtime act = do
  conn <- connectPostgreSQL (connstring runtime)
  trace runtime (Backend SQLConnect)
  !ret <- act conn
  close conn
  pure ret

traceTransaction :: Runtime -> Connection -> IO b -> IO b
traceTransaction runtime conn act =
  withTransaction conn $ do
    trace runtime (Backend SQLTransaction)
    act

traceAttempt :: MonadIO m => Runtime -> LoginAttempt -> m ()
traceAttempt runtime l =
  liftIO $ trace runtime $ Behaviour $ Attempt l

traceVerification :: MonadIO m => Runtime -> Bool -> m ()
traceVerification runtime bool =
  liftIO $ trace runtime $ Behaviour $ Verification bool

traceOptionalVerification :: MonadIO m => Runtime -> Bool -> m ()
traceOptionalVerification runtime bool =
  liftIO $ trace runtime $ Behaviour $ OptionalVerification bool

traceAllowed,traceDisallowed,traceLimited :: MonadIO m => Runtime -> m ()
traceAllowed runtime =
  liftIO $ trace runtime $ Bearer $ Allowed (Just True)
traceDisallowed runtime =
  liftIO $ trace runtime $ Bearer $ Allowed (Just False)
traceLimited runtime =
  liftIO $ trace runtime $ Bearer $ Allowed Nothing

traceJWT :: MonadIO m => Runtime -> Maybe (JWT (VerifiedJWT)) -> m ()
traceJWT runtime jwt =
  liftIO $ trace runtime $ Bearer $ Extracted jwt

tokenValidityDuration :: Minutes
tokenValidityDuration = 30
