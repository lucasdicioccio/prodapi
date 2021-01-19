
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
    Attempt LoginAttempt
  | Verification Bool
  | OptionalVerification Bool

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
