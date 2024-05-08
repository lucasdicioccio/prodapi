{-# LANGUAGE TypeFamilies #-}

module Prod.UserAuth.Api (
    UserAuthApi,
    CookieProtect,
)
where

import Data.Text (Text)
import Prod.UserAuth.Base
import Prod.UserAuth.JWT
import Servant
import Servant.Server.Experimental.Auth (AuthServerData)
import Web.JWT

type CookieProtect = AuthProtect "prod-user-auth"

type instance AuthServerData (AuthProtect "prod-user-auth") = UserAuthInfo

type UserAuthApi a =
    EchoCookieClaimsApi
        :<|> WhoAmIApi a
        :<|> RenewCookieApi
        :<|> CleanCookieApi
        :<|> RegisterApi a
        :<|> LoginApi a
        :<|> RequestRecoveryApi
        :<|> ApplyRecoveryApi
        :<|> HelloProtectedApi

type EchoCookieClaimsApi =
    Summary "echo cookie claims after validating it"
        :> "user-auth"
        :> "echo-cookie"
        :> Header "Cookie" LoggedInCookie
        :> Get '[JSON] JWTClaimsSet

type RenewCookieApi =
    Summary "renews a cookie"
        :> "user-auth"
        :> "renew"
        :> Header "Cookie" LoggedInCookie
        :> Post '[JSON] (Headers '[Header "Set-Cookie" LoggedInCookie] Text)

type WhoAmIApi a =
    Summary "prints user identities for a cookie"
        :> "user-auth"
        :> "whoami"
        :> Header "Cookie" LoggedInCookie
        :> Get '[JSON] [WhoAmI a]

type CleanCookieApi =
    Summary "deletes the cookie"
        :> "user-auth"
        :> "clean-cookie"
        :> Post '[JSON] (Headers '[Header "Set-Cookie" LoggedInCookie] ())

type RegisterApi a =
    Summary "register a new user"
        :> "user-auth"
        :> "registration"
        :> ReqBody '[FormUrlEncoded, JSON] RegistrationRequest
        :> Post '[JSON] (Headers '[Header "Set-Cookie" LoggedInCookie] (RegistrationResult a))

type LoginApi a =
    Summary "login as a user"
        :> "user-auth"
        :> "login"
        :> ReqBody '[FormUrlEncoded, JSON] LoginAttempt
        :> Post '[JSON] (Headers '[Header "Set-Cookie" LoggedInCookie] (LoginResult a))

type RequestRecoveryApi =
    Summary "request an out-of-band password recovery"
        :> "user-auth"
        :> "recovery"
        :> "request"
        :> ReqBody '[FormUrlEncoded, JSON] RecoveryRequest
        :> Post '[JSON] RecoveryRequestNotification

type ApplyRecoveryApi =
    Summary "overwrites the password"
        :> "user-auth"
        :> "recovery"
        :> "apply"
        :> ReqBody '[FormUrlEncoded, JSON] ApplyRecoveryRequest
        :> Post '[JSON] RecoveryResult

type HelloProtectedApi =
    Summary "Hello World with Auth Protect"
        :> CookieProtect
        :> "user-auth"
        :> "hello"
        :> Get '[JSON] Text
