{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prod.UserAuth.Backend where

import Control.Monad ((<=<))
import Data.Maybe (catMaybes)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query, rollback, formatQuery)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Prod.UserAuth.Base
import Prod.UserAuth.Runtime (Runtime, augmentSession, augmentWhoAmI, tokenValidityDuration, withConn, traceTransaction, trace)
import Prod.UserAuth.Trace


registerIO :: Runtime a -> RegistrationRequest -> IO (RegistrationResult a)
registerIO rt req = do
  withConn rt $ \conn -> do
    traceTransaction rt conn $ do
      newU <- newuser rt conn
      case newU of
        [Only uid] -> do
          nres <- newpass conn =<< mkNewPass uid
          if nres /= 1
            then rollback conn >> pure RegisterFailure
            else do
              info <- augmentSession rt conn (SessionData uid ())
              case info of
                Just extra -> pure $ RegisterSuccess (SessionData uid extra)
                Nothing -> pure RegisterFailure
        _ -> pure RegisterFailure
  where
    emailV :: Text
    emailV = req.email
    plainV :: Text
    plainV = req.plain
    mkNewPass uid = pure $ SetPassword uid emailV plainV

type User = Only UserId

newuser :: Runtime a -> Connection -> IO [User]
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

finduser :: Runtime a -> Connection -> Email -> IO [User]
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

finduidMail :: Runtime a -> Connection -> UserId -> IO [Only Email]
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
      LIMIT 1
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
    emailV = pass.email
    plainV = pass.plain
    uidV = pass.uid
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
    emailV = pass.email
    plainV = pass.plain
    uidV = pass.uid
    q =
      [sql|
        UPDATE passwords
        SET hashed = encode(digest(? || x.xsalt, 'sha256'), 'hex')
          , salt = x.xsalt
        FROM (SELECT md5(random()::text) AS xsalt) x
        WHERE (identity_id = ?)
          AND (email = ?)
      |]

loginIO :: Runtime info -> LoginAttempt -> IO (LoginResult info)
loginIO rt attempt = withConn rt (\conn -> login rt conn attempt)

login :: forall info. Runtime info -> Connection -> LoginAttempt -> IO (LoginResult info)
login rt conn attempt = do
    trace rt . Backend . SQLQuery =<< formatQuery conn q (plainV, emailV)
    toResult conn =<< query conn q (plainV, emailV)
  where
    toResult :: Connection -> [(UserId, Bool)] -> IO (LoginResult info)
    toResult conn [(uid, True)] = do
      info <- augmentSession rt conn (SessionData uid ())
      case info of
        Just extra -> pure $ LoginSuccess (SessionData uid extra)
        Nothing -> pure LoginFailed
    toResult _ _ = pure LoginFailed
    plainV = attempt.plain
    emailV = attempt.email
    q =
      [sql|
      SELECT identity_id
           , encode(digest( ? || salt, 'sha256'), 'hex') = hashed
      FROM passwords
      WHERE email = ?
        AND enabled
    |]

data NewRecovery
  = NewRecovery
      { uid :: UserId
      }

type Token = Only TokenValue

tokenValue :: Token -> TokenValue
tokenValue (Only v) = v

newrecovery :: Connection -> NewRecovery -> IO [Token]
newrecovery conn recover = query conn q (Only uidV)
  where
    uidV = recover.uid
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
    uidV = check.uid
    tokenV = check.token
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
    uidV = check.uid
    q =
      [sql|
        UPDATE password_lost_request
        SET used_at = CURRENT_TIMESTAMP
        WHERE (identity_id = ?)
          AND (used_at IS NULL)
      |]

whoAmIQueryIO :: forall info. Runtime info -> UserId -> IO [WhoAmI info]
whoAmIQueryIO rt uid = do
  withConn rt $ \conn -> do
    emails <- finduidMail rt conn uid
    catMaybes <$> traverse (unwrapmail conn) emails
  where
    unwrapmail :: Connection -> Only Email -> IO (Maybe (WhoAmI info))
    unwrapmail conn (Only eml) = do
      info <- augmentWhoAmI rt conn (WhoAmI eml uid)
      case info of
        Just extra -> pure $ Just $ WhoAmI eml extra
        Nothing -> pure Nothing

recoveryRequestIO :: Runtime a -> RecoveryRequest -> IO [Token]
recoveryRequestIO rt req = do
  withConn rt $ \conn -> do
    traceTransaction rt conn $ do
      newU <- finduser rt conn emailV
      case newU of
        [Only uid] -> newrecovery conn (NewRecovery uid)
        _ -> pure []
  where
    emailV :: Text
    emailV = req.email

applyRecoveryIO :: Runtime a -> ApplyRecoveryRequest -> IO RecoveryResult
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
    emailV = req.email
    plainV :: Text
    plainV = req.plain
    tokenV :: Text
    tokenV = req.token
    mkCheckRecovery uid = pure $ CheckRecovery uid tokenV
    mkSetPass uid = pure $ SetPassword uid emailV plainV
