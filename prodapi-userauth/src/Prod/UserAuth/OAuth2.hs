{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Helpers to build OAuth2 callbacks to implement "log-in with Google, GitHub etc".
--
-- Unlike other Runtimes, this Runtime is more of a template than a full.
-- A library user must pas a number of
-- We consider two different states:
-- - the transientState (a unique state to recognize a callback, and passed as query-params)
-- - the finalState (after the user has been redirected to your application)
-- you may exchange the CallbackCode for a token, store it, associate it with a
-- user, perform lookups on the third-party before returning (usually with yet another redirect).
module Prod.UserAuth.OAuth2 where

import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Control.Monad.IO.Class (liftIO)
import Servant
import Prod.Tracer as Tracer

newtype CallbackCode = CallbackCode Text
  deriving (FromHttpApiData)

data Track transientState finalState
  = OAuth2Initialization transientState
  | OAuth2Calback transientState (CallbackResult finalState)
  deriving Show

-- | Proves that an authorization has been initiated, carrying OAuth2 redirect
-- to the IDP and some state to encode and link the whole flow.
data AuthorizeStateProof transientState = AuthorizeStateProof
  { transientState :: transientState
  , initialRedirect :: Text
  }
  deriving Show

-- | Proves that the OAuth2 IDP redirected the user to your application.
data ProofOfProcessedToken finalState = ProofOfProcessedToken
  { finalState :: finalState
  }
  deriving Show

data CallbackResult finalState
 = UnrecognizedState 
 | OtherError Text
 | Success (ProofOfProcessedToken finalState)
  deriving Show

data Runtime transientState finalState = Runtime
  { tracer  :: Tracer IO (Track transientState finalState)
  , initiateState :: IO (AuthorizeStateProof transientState)
  , requestToken :: transientState -> CallbackCode -> IO (CallbackResult finalState)
  }

type Oauth2InitiateRedirectApi transientState
  = "oauth2"
  :> "login"
  :> Get '[JSON] ()

type Oauth2CallbackApi transientState
  = "oauth2"
  :> "callback"
  :> QueryParam' '[Required] "state" transientState
  :> QueryParam' '[Required] "code" CallbackCode
  :> Get '[JSON] ()

handleOAuth2InitiateRedirect
  :: Runtime transientState finalState
  -> Handler ()
handleOAuth2InitiateRedirect rt = do
  p <- liftIO go
  throwError err302 { errHeaders = [("Location", Text.encodeUtf8 p.initialRedirect)] }
  where
    go = do
      p <- rt.initiateState
      Tracer.runTracer rt.tracer (OAuth2Initialization p.transientState)
      pure p

handleOAuth2Callback
  :: Runtime transientState finalState
  -> (finalState -> Handler ())
  -> transientState
  -> CallbackCode
  -> Handler ()
handleOAuth2Callback rt finalize transientState code = do
  res <- liftIO go
  case res of
    UnrecognizedState ->
      throwError err400 { errBody = "invalid oauth2 callback transientState" }
    OtherError _ ->
      throwError err500
    Success p ->
      finalize p.finalState
  where
    go = do
      p <- rt.requestToken transientState code
      Tracer.runTracer rt.tracer (OAuth2Calback transientState p)
      pure p

type OAuth2MinimalApi transientState
  = Oauth2InitiateRedirectApi transientState
  :<|> Oauth2CallbackApi transientState

serveMinimalApi
  :: Runtime transientState finalState
  -> (finalState -> Handler())
  -> Server (OAuth2MinimalApi transientState)
serveMinimalApi rt finalize =
  handleOAuth2InitiateRedirect rt
  :<|> handleOAuth2Callback rt finalize
