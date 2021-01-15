
module Prod.UserAuth.Trace where

import Data.ByteString (ByteString)
import Prod.UserAuth.Base

data Track =
    Behaviour BehaviourTrack
  | Backend BackendTrack

-- | Track enough information to study potentially-malicious activities.
data BehaviourTrack =
    Attempt LoginAttempt
  | Result LoginResult
  | Verification Bool
  | OptionalVerification Bool
  | Allowed (Maybe Bool)
  | Register RegistrationRequest
  | Recover RecoveryRequest
  | Recovered RecoveryResult

-- | Track backend sql information.
data BackendTrack =
    SQLConnect
  | SQLTransaction
  | SQLRollback
  | SQLQuery !ByteString
