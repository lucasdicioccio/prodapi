module Prod.UserAuth.HandlerCombinators
  ( authServerContext,
    authorized,
    limited,
    withLoginCookieVerified,
    withOptionalLoginCookieVerified,
    Request,
  )
where

import Data.Text.Encoding (decodeLatin1)
import Network.Wai (Request, requestHeaders)
import Prod.UserAuth.Base
import Prod.UserAuth.JWT
import Prod.UserAuth.Runtime
import Servant
import Servant.Server
import Servant.Server.Experimental.Auth
  ( AuthHandler,
    mkAuthHandler,
  )
import Web.Cookie (parseCookies)

authHandler :: Runtime -> AuthHandler Request UserAuthInfo
authHandler runtime = mkAuthHandler go
  where
    go req = do
      let mCookies = fmap parseCookies $ lookup "cookie" $ requestHeaders req
      let jwtblob = fmap decodeLatin1 $ lookup "login-jwt" =<< mCookies
      let mJwt = decodeAndVerifySignature (hmacSecret $ secretstring runtime) =<< jwtblob
      pure $ UserAuthInfo mJwt

authServerContext :: Runtime -> Context (AuthHandler Request UserAuthInfo ': '[])
authServerContext runtime =
  authHandler runtime :. EmptyContext

withLoginCookieVerified ::
  Runtime ->
  Maybe LoggedInCookie ->
  (JWT VerifiedJWT -> Handler a) ->
  Handler a
withLoginCookieVerified _ Nothing _ =
  throwError $ err403 {errBody = "Sorry, need some cookies."}
withLoginCookieVerified runtime cookie act = do
  withOptionalLoginCookieVerified runtime cookie $ \mJwt ->
    maybe (throwError err403) act mJwt

withOptionalLoginCookieVerified ::
  Runtime ->
  Maybe LoggedInCookie ->
  (Maybe (JWT VerifiedJWT) -> Handler a) ->
  Handler a
withOptionalLoginCookieVerified runtime cookie act = do
  let mJwt = decodeAndVerifySignature (hmacSecret $ secretstring runtime) =<< fmap encodedJwt cookie
  act mJwt

authorized :: UserAuthInfo -> (UserId -> Handler a) -> Handler a
authorized auth act =
  limited auth (throwError err401) act

limited :: UserAuthInfo -> (Handler a) -> (UserId -> Handler a) -> Handler a
limited auth fallback act =
  maybe fallback act (authUserId auth)
