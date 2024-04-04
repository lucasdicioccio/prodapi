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
import Web.FormUrlEncoded

data RecoveryResult = RecoverySuccess | RecoveryFailed Text
    deriving (Show, Generic)

instance ToJSON RecoveryResult

type TokenValue = Text

data ApplyRecoveryRequest
    = ApplyRecoveryRequest
    { email :: Text
    , plain :: Text
    , token :: TokenValue
    }
    deriving (Generic)
instance Show ApplyRecoveryRequest where
    showsPrec _ x = (\x -> "ApplyRecoveryRequest<hidden>" <> x)

instance FromJSON ApplyRecoveryRequest
instance FromForm ApplyRecoveryRequest

data LoggedInCookie = LoggedInCookie {encodedJwt :: !Text}
    deriving (Generic)
instance Show LoggedInCookie where
    showsPrec _ x = (\x -> "LoggedInCookie<hidden>" <> x)

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
        mconcat [loginjwtCookiePrefix, txt, "; Path=/; SameSite=Strict; HttpOnly; Secure"]

data WhoAmI info
    = WhoAmI
    { email :: Maybe Text
    , info :: info
    }
    deriving (Generic)

instance (ToJSON info) => ToJSON (WhoAmI info)

data RegistrationRequest
    = RegistrationRequest
    { email :: Text
    , plain :: Text
    }
    deriving (Generic)

instance Show RegistrationRequest where
    showsPrec _ x = (\x -> "RegistrationRequest<hidden>" <> x)

instance FromJSON RegistrationRequest
instance FromForm RegistrationRequest

data RegistrationResult a = RegisterSuccess (SessionData a) | RegisterFailure
    deriving (Generic, Show)

instance (ToJSON a) => ToJSON (RegistrationResult a)

data SessionData a
    = SessionData
    { userId :: UserId
    , info :: a
    }
    deriving (Generic, Show)

instance (ToJSON a) => ToJSON (SessionData a)

type UserId = Int64

type Minutes = Int

data RecoveryRequest
    = RecoveryRequest
    { email :: Text
    }
    deriving (Generic)

instance Show RecoveryRequest where
    showsPrec _ x = (\x -> "RecoveryRequest<hidden>" <> x)

instance FromJSON RecoveryRequest
instance FromForm RecoveryRequest

data RecoveryRequestNotification
    = RecoveryRequestNotification
    { email :: Text
    , minutes :: Minutes
    , token :: TokenValue
    }
    deriving (Generic)

instance ToJSON RecoveryRequestNotification

instance Show RecoveryRequestNotification where
    showsPrec _ x = (\x -> "RecoveryRequestNotification<hidden>" <> x)

data LoginAttempt
    = LoginAttempt
    { email :: Text
    , plain :: Text
    }
    deriving (Generic)

instance Show LoginAttempt where
    showsPrec _ x = (\x -> "LoginAttempt<hidden>" <> x)

instance FromJSON LoginAttempt
instance FromForm LoginAttempt

data LoginResult a = LoginSuccess (SessionData a) | LoginFailed
    deriving (Generic, Show)

instance (ToJSON a) => ToJSON (LoginResult a)

type ErrorMessage = Text
