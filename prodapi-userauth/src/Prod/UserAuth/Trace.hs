
module Prod.UserAuth.Trace where

import Data.ByteString (ByteString)
import Prod.UserAuth.Base
import Web.JWT

data Track info =
    Behaviour (BehaviourTrack info)
  | Bearer JwtTrack
  | Backend BackendTrack

-- | Track enough information to study potentially-malicious activities.
data BehaviourTrack info =
    Attempt LoginAttempt (LoginResult info)
  | Verification Bool
  | OptionalVerification Bool
  | Registration RegistrationRequest (RegistrationResult info)
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
