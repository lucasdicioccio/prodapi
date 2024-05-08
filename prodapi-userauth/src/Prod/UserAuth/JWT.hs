{-# LANGUAGE StrictData #-}

module Prod.UserAuth.JWT (
    UserAuthInfo (..),
    authUserId,
    makeLoggedInCookie,
    module Web.JWT,
)
where

import Data.Aeson (FromJSON, Result (..), Value (Number), fromJSON)
import Data.Fixed (E12, Fixed (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
import Prod.UserAuth.Base
import Prod.UserAuth.Runtime
import Web.JWT

data UserAuthInfo = UserAuthInfo (Maybe (JWT VerifiedJWT))

-- | Extract an decodes a JSON value from a UserAuthInfo containing a VerifiedJWT.
authClaim :: (FromJSON a) => UserAuthInfo -> Text -> Maybe a
authClaim (UserAuthInfo mjwt) key = do
    jwt <- mjwt
    obj <- Map.lookup key (unClaimsMap . unregisteredClaims $ claims jwt)
    case fromJSON obj of
        Error _ -> Nothing
        Success x -> Just x

-- | Extract a JSON value from a UserAuthInfo containing a VerifiedJWT.
authClaimValue :: UserAuthInfo -> Text -> Maybe Value
authClaimValue (UserAuthInfo mjwt) key = do
    jwt <- mjwt
    Map.lookup key (unClaimsMap . unregisteredClaims $ claims jwt)

-- | Extracts the UserId from a UserAuthInfo containing VerifiedJWT.
authUserId :: UserAuthInfo -> Maybe UserId
authUserId uai = do
    val <- authClaim uai "user-id"
    case val of
        (Number nid) -> pure (truncate nid :: UserId)
        _ -> Nothing

{- | Encodes a JWT cookie for a given UserId.

The runtime allows augmenting the JWT with extra claims.
-}
makeLoggedInCookie :: Runtime a -> UserId -> IO (Either ErrorMessage LoggedInCookie)
makeLoggedInCookie runtime uid = do
    tNow <- getPOSIXTime
    let times = (,) <$> (numericDate tNow) <*> (numericDate $ tNow + secondsToNominalDiffTime 3600)
    case times of
        Nothing -> pure $ Left "could not build a valid issued-at"
        Just (iat, exp) -> do
            extras <- augmentLoggedInCookieClaims runtime uid
            case extras of
                Left err -> pure $ Left err
                Right vals -> do
                    traceJWTSigned runtime uid vals
                    pure $ Right $ adapt iat exp vals
  where
    baseClaims = baseClaimsSet runtime
    adapt iat exp extras = do
        let claims = Map.fromList $ mconcat [[("user-id", (Number $ fromIntegral uid))], extras]
        LoggedInCookie $
            encodeSigned
                (hmacSecret $ secretstring runtime)
                mempty
                ( baseClaims
                    { iss = stringOrURI "jwt-app"
                    , iat = Just iat
                    , Web.JWT.exp = Just exp
                    , unregisteredClaims = ClaimsMap claims
                    }
                )
