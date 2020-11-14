{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Prod.UserAuth.Base where

import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON, Value (Number))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Servant

data RecoveryResult = RecoverySuccess | RecoveryFailed Text
  deriving (Show, Generic)

instance ToJSON RecoveryResult

type TokenValue = Text

data ApplyRecoveryRequest
  = ApplyRecoveryRequest
      { email :: Text,
        plain :: Text,
        token :: TokenValue
      }
  deriving (Generic)

instance FromJSON ApplyRecoveryRequest

data LoggedInCookie = LoggedInCookie {encodedJwt :: !Text}
  deriving (Generic)

instance ToJSON LoggedInCookie

instance FromHttpApiData LoggedInCookie where
  parseUrlPiece = locateJWT
    where
      locateJWT :: Text -> Either Text LoggedInCookie
      locateJWT = safeHead . locateJWTlist
      safeHead :: [Text] -> Either Text LoggedInCookie
      safeHead (x : _) = Right (LoggedInCookie x)
      safeHead _ = Left "invalid login-jwt cookie key"
      locateJWTlist :: Text -> [Text]
      locateJWTlist dat = do
        w <- Text.splitOn ";" dat
        guard $ loginjwtCookiePrefix `Text.isPrefixOf` w
        pure $ Text.drop (Text.length loginjwtCookiePrefix) w

loginjwtCookiePrefix :: Text
loginjwtCookiePrefix = "login-jwt="

instance ToHttpApiData LoggedInCookie where
  toUrlPiece (LoggedInCookie txt) =
    mconcat [loginjwtCookiePrefix, txt, "; Path=/; SameSite=Strict; HttpOnly"]

data WhoAmI
  = WhoAmI
      { email :: Text
      }
  deriving (Generic)

instance ToJSON WhoAmI

data RegistrationRequest
  = RegistrationRequest
      { email :: Text,
        plain :: Text
      }
  deriving (Generic)

instance FromJSON RegistrationRequest

data RegistrationResult = RegisterSuccess SessionData | RegisterFailure
  deriving (Generic)

instance ToJSON RegistrationResult

data SessionData
  = SessionData
      { userId :: UserId
      }
  deriving (Generic)

instance ToJSON SessionData

type UserId = Int64

type Minutes = Int

data RecoveryRequest
  = RecoveryRequest
      { email :: Text
      }
  deriving (Generic)

instance FromJSON RecoveryRequest

data RecoveryRequestNotification
  = RecoveryRequestNotification
      { email :: Text,
        minutes :: Minutes
      }
  deriving (Generic)

instance ToJSON RecoveryRequestNotification

data LoginAttempt
  = LoginAttempt
      { email :: Text,
        plain :: Text
      }
  deriving (Generic)

instance FromJSON LoginAttempt

data LoginResult = LoginSuccess SessionData | LoginFailed
  deriving (Generic)

instance ToJSON LoginResult
