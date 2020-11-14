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

type StatusApi =
  "status"
    :> Get '[JSON] Status

newtype Identification = Identification Text
  deriving
    (ToJSON)
    via Text

data Status
  = Status
      { identification :: !Identification,
        liveness :: !Liveness,
        readiness :: !Readiness
      }
  deriving (Generic)

instance ToJSON Status

handleStatus :: Runtime -> Handler Status
handleStatus runtime =
  liftIO $
    Status this
      <$> Health.liveness runtime
      <*> Health.readiness runtime

{-# NOINLINE this #-}
this :: Identification
this = unsafePerformIO $ fmap (Identification . Text.pack . show) nextRandom
