module Prod.UserAuth.Trace where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Prod.UserAuth.Base
import Web.JWT

data Track info
    = Behaviour (BehaviourTrack info)
    | Bearer JwtTrack
    | Backend BackendTrack
    | Callbacks CallbackTrack
    deriving (Show)

-- | Track enough information to study potentially-malicious activities.
data BehaviourTrack info
    = Attempt LoginAttempt (LoginResult info)
    | Verification Bool
    | OptionalVerification Bool
    | Registration RegistrationRequest (RegistrationResult info)
    | Recovery RecoveryRequest RecoveryRequestNotification
    | ApplyRecovery ApplyRecoveryRequest RecoveryResult
    deriving (Show)

-- | Bearer tokens with JWT.
data JwtTrack
    = Extracted (Maybe (JWT VerifiedJWT))
    | Allowed (Maybe Bool)
    | Signed UserId [(Text, Value)]
    deriving (Show)

-- | Track backend sql information.
data BackendTrack
    = SQLConnect
    | SQLTransaction
    | SQLRollback
    | SQLQuery !ByteString
    deriving (Show)

-- | Track callback information.
data CallbackTrack
    = AugmentCookie (Either ErrorMessage LoggedInCookie)
    deriving (Show)
