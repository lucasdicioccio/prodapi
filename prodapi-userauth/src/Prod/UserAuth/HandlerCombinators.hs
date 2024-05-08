module Prod.UserAuth.HandlerCombinators (
    authServerContext,
    authorized,
    limited,
    withLoginCookieVerified,
    withOptionalLoginCookieVerified,
    Request,
)
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock.POSIX
import Network.Wai (Request, requestHeaders)
import Prod.UserAuth.Base
import Prod.UserAuth.JWT
import Prod.UserAuth.Runtime
import Servant
import Servant.Server
import Servant.Server.Experimental.Auth (
    AuthHandler,
    mkAuthHandler,
 )
import Web.Cookie (parseCookies)
import qualified Web.JWT

verifyExpiryClaim :: Maybe (JWT a) -> IO (Maybe (JWT a))
verifyExpiryClaim jwt = case fmap claims jwt >>= Web.JWT.exp of
    Nothing -> pure Nothing -- no expiry
    (Just expdate) -> do
        now <- Web.JWT.numericDate <$> getPOSIXTime
        case now of
            Nothing -> pure Nothing -- could not get a current time
            (Just nowdate)
                | nowdate < expdate -> pure jwt -- there still is some time left
                | otherwise -> pure Nothing -- expired

authHandler :: Runtime a -> AuthHandler Request UserAuthInfo
authHandler runtime = mkAuthHandler go
  where
    go req = do
        let mCookies = fmap parseCookies $ lookup "cookie" $ requestHeaders req
        let jwtblob = fmap decodeLatin1 $ lookup "login-jwt" =<< mCookies
        let mJwt = decodeAndVerifySignature (toVerify . hmacSecret $ secretstring runtime) =<< jwtblob
        validJwt <- liftIO $ verifyExpiryClaim mJwt
        traceJWT runtime validJwt
        pure $ UserAuthInfo validJwt

authServerContext :: Runtime a -> Context (AuthHandler Request UserAuthInfo ': '[])
authServerContext runtime =
    authHandler runtime :. EmptyContext

withLoginCookieVerified ::
    Runtime info ->
    Maybe LoggedInCookie ->
    (JWT VerifiedJWT -> Handler a) ->
    Handler a
withLoginCookieVerified runtime Nothing _ = do
    traceVerification runtime False
    throwError $ err403{errBody = "Sorry, need some cookies."}
withLoginCookieVerified runtime cookie act = do
    withOptionalLoginCookieVerified runtime cookie $ \mJwt ->
        maybe
            (traceVerification runtime False >> throwError err403)
            (\x -> traceVerification runtime True >> act x)
            mJwt

withOptionalLoginCookieVerified ::
    Runtime info ->
    Maybe LoggedInCookie ->
    (Maybe (JWT VerifiedJWT) -> Handler a) ->
    Handler a
withOptionalLoginCookieVerified runtime cookie act = do
    let mJwt = decodeAndVerifySignature (toVerify . hmacSecret $ secretstring runtime) =<< fmap encodedJwt cookie
    validJwt <- liftIO $ verifyExpiryClaim mJwt
    traceOptionalVerification runtime (isJust validJwt)
    act validJwt

authorized :: Runtime info -> UserAuthInfo -> (UserId -> Handler a) -> Handler a
authorized rt auth act =
    limited rt auth (traceDisallowed rt >> throwError err401) act

limited :: Runtime info -> UserAuthInfo -> (Handler a) -> (UserId -> Handler a) -> Handler a
limited rt auth fallback act =
    maybe (traceLimited rt >> fallback) (\uid -> traceAllowed rt >> act uid) (authUserId auth)
