{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Prod.UserAuth.Backend where

import Control.Monad ((<=<))
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, rollback, formatQuery)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Prod.UserAuth.Base
import Prod.UserAuth.Runtime (Runtime, tokenValidityDuration, withConn, traceTransaction, trace)
import Prod.UserAuth.Trace

registerIO :: Runtime -> RegistrationRequest -> IO RegistrationResult
registerIO rt req = do
  withConn rt $ \conn -> do
    traceTransaction rt conn $ do
      newU <- newuser rt conn
      case newU of
        [Only uid] -> do
          nres <- newpass conn =<< mkNewPass uid
          if nres /= 1
            then rollback conn >> pure RegisterFailure
            else pure $ RegisterSuccess (SessionData uid)
        _ -> pure RegisterFailure
  where
    emailV :: Text
    emailV = email (req :: RegistrationRequest)
    plainV :: Text
    plainV = plain (req :: RegistrationRequest)
    mkNewPass uid = pure $ SetPassword uid emailV plainV

type User = Only UserId

newuser :: Runtime -> Connection -> IO [User]
newuser rt conn = do
    trace rt . Backend . SQLQuery =<< formatQuery conn q ()
    query conn q ()
  where
    q =
      [sql|
      INSERT INTO identities(id)
      VALUES (DEFAULT)
      RETURNING (id)
    |]

type Email = Text

finduser :: Runtime -> Connection -> Email -> IO [User]
finduser rt conn email = do
    trace rt . Backend . SQLQuery =<< formatQuery conn q (Only email)
    query conn q (Only email)
  where
    q =
      [sql|
      SELECT identity_id
      FROM passwords
      WHERE email = ?
        AND enabled
    |]

finduidMail :: Runtime -> Connection -> UserId -> IO [Only Email]
finduidMail rt conn uid = do
   trace rt . Backend . SQLQuery =<< formatQuery conn q (Only uid)
   query conn q (Only uid)
  where
    q =
      [sql|
      SELECT email
      FROM passwords
      WHERE identity_id = ?
        AND enabled
    |]

data SetPassword
  = SetPassword
      { uid :: UserId,
        email :: Text,
        plain :: Text
      }

-- | Saves a new password.
newpass :: Connection -> SetPassword -> IO Int64
newpass conn pass = execute conn q (emailV, plainV, uidV)
  where
    emailV = email (pass :: SetPassword)
    plainV = plain (pass :: SetPassword)
    uidV = uid (pass :: SetPassword)
    q =
      [sql|
      INSERT INTO passwords(email,enabled,hashed,salt,identity_id)
      SELECT ?, true, encode(digest(? || x.xsalt, 'sha256'), 'hex'), x.xsalt, ?
        FROM (SELECT md5(random()::text) AS xsalt) x
    |]

-- | Resets a password.
resetpass :: Connection -> SetPassword -> IO Int64
resetpass conn pass = execute conn q (plainV, uidV, emailV)
  where
    emailV = email (pass :: SetPassword)
    plainV = plain (pass :: SetPassword)
    uidV = uid (pass :: SetPassword)
    q =
      [sql|
        UPDATE passwords
        SET hashed = encode(digest(? || x.xsalt, 'sha256'), 'hex')
          , salt = x.xsalt
        FROM (SELECT md5(random()::text) AS xsalt) x
        WHERE (identity_id = ?)
          AND (email = ?)
      |]

loginIO :: Runtime -> LoginAttempt -> IO LoginResult
loginIO rt attempt = withConn rt (\conn -> login rt conn attempt)

login :: Runtime -> Connection -> LoginAttempt -> IO LoginResult
login rt conn attempt = do
    trace rt . Backend . SQLQuery =<< formatQuery conn q (plainV, emailV)
    toResult <$> query conn q (plainV, emailV)
  where
    toResult :: [(UserId, Bool)] -> LoginResult
    toResult [(uid, True)] = LoginSuccess (SessionData uid)
    toResult _ = LoginFailed
    plainV = plain (attempt :: LoginAttempt)
    emailV = email (attempt :: LoginAttempt)
    q =
      [sql|
      SELECT identity_id
           , encode(digest( ? || salt, 'sha256'), 'hex') = hashed
      FROM passwords
      WHERE email = ?
        AND enabled
      LIMIT 1
    |]

data NewRecovery
  = NewRecovery
      { uid :: UserId
      }

type Token = Only TokenValue

newrecovery :: Connection -> NewRecovery -> IO [Token]
newrecovery conn recover = query conn q (Only uidV)
  where
    uidV = uid (recover :: NewRecovery)
    q =
      [sql|
        INSERT INTO password_lost_request(timestamp,identity_id,token)
        SELECT CURRENT_TIMESTAMP, ?, md5(random()::text)
        RETURNING token
      |]

data CheckRecovery
  = CheckRecovery
      { uid :: UserId,
        token :: TokenValue
      }

checkrecovery :: Connection -> CheckRecovery -> IO RecoveryResult
checkrecovery conn check =
  toResult <$> query conn q (tokenValidityDuration :: Minutes, uidV, tokenV)
  where
    toResult :: [Only Bool] -> RecoveryResult
    toResult [Only True] = RecoverySuccess
    toResult _ = RecoveryFailed "invalid-checkrecovery-result"
    uidV = uid (check :: CheckRecovery)
    tokenV = token (check :: CheckRecovery)
    q =
      [sql|
        SELECT age(CURRENT_TIMESTAMP, timestamp) <= '? min'
        FROM password_lost_request
        WHERE (identity_id = ?)
          AND (token = ?)
          AND (used_at IS NULL)
        LIMIT 1
      |]

invalidaterecovery :: Connection -> CheckRecovery -> IO Int64
invalidaterecovery conn check = execute conn q (Only uidV)
  where
    uidV = uid (check :: CheckRecovery)
    q =
      [sql|
        UPDATE password_lost_request
        SET used_at = CURRENT_TIMESTAMP
        WHERE (identity_id = ?)
          AND (used_at IS NULL)
      |]

whoAmIQueryIO :: Runtime -> UserId -> IO [WhoAmI]
whoAmIQueryIO rt uid = do
  withConn rt $ \conn -> do
    fmap unwrapmail <$> finduidMail rt conn uid
  where
    unwrapmail :: Only Email -> WhoAmI
    unwrapmail (Only pass) = WhoAmI pass

recoveryRequestIO :: Runtime -> RecoveryRequest -> IO [Token]
recoveryRequestIO rt req = do
  withConn rt $ \conn -> do
    traceTransaction rt conn $ do
      newU <- finduser rt conn emailV
      case newU of
        [Only uid] -> newrecovery conn (NewRecovery uid)
        _ -> pure []
  where
    emailV :: Text
    emailV = email (req :: RecoveryRequest)

applyRecoveryIO :: Runtime -> ApplyRecoveryRequest -> IO RecoveryResult
applyRecoveryIO rt req = do
  withConn rt $ \conn -> do
    traceTransaction rt conn $ do
      newU <- finduser rt conn emailV
      case newU of
        [Only uid] -> do
          recovery <- mkCheckRecovery uid
          canRecover <- checkrecovery conn recovery
          case canRecover of
            RecoverySuccess -> do
              nrec <- invalidaterecovery conn recovery
              nres <- resetpass conn =<< mkSetPass uid
              if (nrec < 1 && nres /= 1)
                then rollback conn >> pure (RecoveryFailed "invariant failed")
                else pure RecoverySuccess
            r -> pure r
        _ -> pure $ RecoveryFailed "unfound user"
  where
    emailV :: Text
    emailV = email (req :: ApplyRecoveryRequest)
    plainV :: Text
    plainV = plain (req :: ApplyRecoveryRequest)
    tokenV :: Text
    tokenV = token (req :: ApplyRecoveryRequest)
    mkCheckRecovery uid = pure $ CheckRecovery uid tokenV
    mkSetPass uid = pure $ SetPassword uid emailV plainV
