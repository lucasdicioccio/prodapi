module Prod.Proxy.Lookups where

import Control.Monad (when)

import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import System.Random.Shuffle (shuffleM)

import Prod.Health (Readiness(..))
import Prod.Proxy.Base

-- | Lookup that always fails.
noBackend :: LookupHostPort
noBackend = const (pure Nothing)

-- | Lookup that always returns the same value.
constBackend :: (Host, Port) -> LookupHostPort
constBackend hp = const (pure $ Just hp)

-- | Lookup the first backend among a generated list.
firstBackend :: IO [(Host, Port)] -> LookupHostPort
firstBackend disc =
   const f
  where
    f :: IO (Maybe (Host, Port))
    f = safeHead <$> disc
 
    safeHead :: [a] -> Maybe a
    safeHead (x:_) = Just x
    safeHead _     = Nothing

-- | Picks a random backend among a generated list.
randomBackend :: IO [(Host,Port)] -> LookupHostPort
randomBackend disc = firstBackend (disc >>= shuffleM)

-- | Fallbacks to the second lookup function if the first fails.
fallback :: LookupHostPort -> LookupHostPort -> LookupHostPort
fallback l1 l2 = \req -> do
  o1 <- l1 req
  case o1 of
    Nothing -> l2 req
    Just _ -> pure o1

-- | Decorates successful lookups with an action.
decorateSuccessWith :: ((Host,Port) -> IO ()) -> LookupHostPort -> LookupHostPort
decorateSuccessWith f l1 = \req -> do
  o1 <- l1 req
  case o1 of
    Nothing -> pure ()
    Just hp -> f hp
  pure o1
