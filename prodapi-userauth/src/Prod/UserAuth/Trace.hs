
module Prod.UserAuth.Trace where

import Data.ByteString (ByteString)
import Prod.UserAuth.Base
import Web.JWT

data Track =
    Behaviour BehaviourTrack
  | Bearer JwtTrack
  | Backend BackendTrack

-- | Track enough information to study potentially-malicious activities.
data BehaviourTrack =
    Attempt LoginAttempt LoginResult
  | Verification Bool
  | OptionalVerification Bool
  | Registration RegistrationRequest RegistrationResult
  | Recovery RecoveryRequest RecoveryRequestNotification
  | ApplyRecovery ApplyRecoveryRequest RecoveryResult

-- | Bearer tokens with JWT.
data JwtTrack =
    Extracted (Maybe (JWT VerifiedJWT))
  | Allowed (Maybe Bool)

-- | Track backend sql information.
data BackendTrack =
    SQLConnect
  | SQLTransaction
  | SQLRollback
  | SQLQuery !ByteString
