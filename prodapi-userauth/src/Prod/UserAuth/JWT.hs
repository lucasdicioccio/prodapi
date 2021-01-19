{-# LANGUAGE StrictData #-}

module Prod.UserAuth.JWT
  ( UserAuthInfo (..),
    authUserId,
    makeLoggedInCookie,
    module Web.JWT,
  )
where

import Data.Aeson (Value (Number))
import qualified Data.Map.Strict as Map
import Prod.UserAuth.Base
import Prod.UserAuth.Runtime
import Web.JWT

data UserAuthInfo = UserAuthInfo (Maybe (JWT VerifiedJWT))

authUserId :: UserAuthInfo -> Maybe UserId
authUserId (UserAuthInfo mjwt) = do
  jwt <- mjwt
  val <- Map.lookup "user-id" (unClaimsMap . unregisteredClaims $ claims jwt)
  case val of
    (Number nid) -> pure (truncate nid :: UserId)
    _ -> Nothing

makeLoggedInCookie :: Runtime -> UserId -> IO LoggedInCookie
makeLoggedInCookie runtime uid = do
  let claims = Map.fromList [("user-id", (Number $ fromIntegral uid))]
  pure $ LoggedInCookie $
    encodeSigned
      (hmacSecret $ secretstring runtime)
      mempty
      ( mempty
          { iss = stringOrURI "jwt-app",
            unregisteredClaims = ClaimsMap claims
          }
      )
