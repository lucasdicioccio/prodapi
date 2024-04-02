module Prod.Gen.Docs.UserAuth where

import Data.Aeson
import Data.Proxy
import Data.Text
import Prod.UserAuth
import Prod.UserAuth.Base
import Prod.UserAuth.JWT
import Servant.API
import Servant.Docs
import Web.Internal.FormUrlEncoded

instance ToForm RegistrationRequest
instance ToJSON RegistrationRequest
instance ToForm LoginAttempt
instance ToJSON LoginAttempt
instance ToForm RecoveryRequest
instance ToJSON RecoveryRequest
instance ToForm ApplyRecoveryRequest
instance ToJSON ApplyRecoveryRequest

instance ToSample JWTClaimsSet where
    toSamples _ =
        [
            ( "some claims set with issuer etc."
            , mempty
                { iss = stringOrURI "issuer"
                , sub = stringOrURI "..."
                }
            )
        ]
instance ToSample () where
    toSamples _ = [("the unit type, representing an absence of return value", ())]
instance ToSample Text where
    toSamples _ = [("some arbitrary text", "lorem ipsum")]
instance ToSample (WhoAmI Text) where
    toSamples _ =
        [ ("i am a robot", WhoAmI (Just ("a robot")) "with some metal head")
        , ("i am a girafe", WhoAmI (Just ("a girafe")) "with some fur")
        ]

instance ToSample LoggedInCookie where
    toSamples _ = [("an encoded JWT", LoggedInCookie example)]
      where
        example = "login-jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJqd3QtYXBwIiwidXNlci1pZCI6NH0.v88HCeDuNsk83umM291-2JT6kgnHYSczld9oU3TnI0s; Path=/; SameSite=Strict; HttpOnly"

instance ToSample RegistrationRequest where
    toSamples _ = [("some registration", RegistrationRequest "foo@example.com" "my desired pass")]
instance ToSample (RegistrationResult Text) where
    toSamples _ =
        [ ("successful registration with user id 1234", RegisterSuccess (SessionData 1234 "the robot"))
        , ("failed registration", RegisterFailure)
        ]
instance ToSample LoginAttempt where
    toSamples _ = [("temptative login", LoginAttempt "foo@example.com" "secret")]
instance ToSample (LoginResult Text) where
    toSamples _ =
        [ ("sucessful login with user id 1234", LoginSuccess (SessionData 1234 "the robot"))
        , ("failed login", LoginFailed)
        ]
instance ToSample RecoveryRequest where
    toSamples _ =
        [ ("recovery request", RecoveryRequest "foo@example.com")
        ]
instance ToSample RecoveryRequestNotification where
    toSamples _ =
        [ ("recovery request notification valid for 60minutes", RecoveryRequestNotification "foo@example.com" 60 "random-bytes")
        ]
instance ToSample ApplyRecoveryRequest where
    toSamples _ = [("apply a token received out of bound", ApplyRecoveryRequest "foo@example.com" "new-password" "secret token received out of bound")]
instance ToSample RecoveryResult where
    toSamples _ =
        [ ("successfully changed pass", RecoverySuccess)
        , ("failed to change pass", RecoveryFailed "some reason")
        ]

instance (HasDocs api) => HasDocs (CookieProtect :> api) where
    docsFor _ (endpoint, action) docopts =
        docsFor (Proxy @api) (endpoint, action) docopts

type UserAuthApiForDocs = UserAuthApi Text

run :: IO ()
run = putStrLn $ markdown $ docs (Proxy @UserAuthApiForDocs)
