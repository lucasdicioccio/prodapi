{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Prod.Status
  ( StatusApi,
    handleStatus,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Prod.Health as Health
import Servant ((:>), Get, JSON)
import Servant.Server (Handler)
import System.IO.Unsafe (unsafePerformIO)

type StatusApi a =
  "status"
    :> Get '[JSON] (Status a)

newtype Identification = Identification Text
  deriving
    (ToJSON)
    via Text

data Status a
  = Status
      { identification :: !Identification,
        liveness :: !Liveness,
        readiness :: !Readiness,
        appStatus :: !a
      }
  deriving (Generic)

instance ToJSON a => ToJSON (Status a)

handleStatus :: Runtime -> IO a -> Handler (Status a)
handleStatus runtime getAppStatus =
  liftIO $
    Status this
      <$> Health.liveness runtime
      <*> Health.readiness runtime
      <*> getAppStatus

{-# NOINLINE this #-}
this :: Identification
this = unsafePerformIO $ fmap (Identification . Text.pack . show) nextRandom
