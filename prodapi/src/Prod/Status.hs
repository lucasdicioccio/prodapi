{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prod.Status
  ( StatusApi,
    handleStatus,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Foldable (traverse_)
import qualified Data.Text as Text
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Prod.Health as Health
import Servant ((:>), Get, JSON)
import Servant.Server (Handler)
import System.IO.Unsafe (unsafePerformIO)
import Servant (MimeRender(..))
import Prod.MimeTypes (HTML)

import Lucid

type StatusApi a =
  "status"
    :> Get '[HTML,JSON] (Status a)

newtype Identification = Identification Text
  deriving
    (ToJSON)
    via Text

type RenderStatus a = Status a -> Html ()

data Status a
  = Status
      { identification :: !Identification,
        liveness :: !Liveness,
        readiness :: !Readiness,
        appStatus :: !a,
        renderer :: RenderStatus a
      }

instance ToJSON a => ToJSON (Status a) where
  toJSON (Status i l r st _) =
    Aeson.object [ "id" .= i
                 , "liveness" .= l
                 , "readiness" .= r
                 , "status" .= toJSON st
                 ]

handleStatus :: Runtime -> IO a -> Handler (Status a)
handleStatus runtime getAppStatus =
  liftIO $
    Status this
      <$> Health.liveness runtime
      <*> Health.readiness runtime
      <*> getAppStatus
      <*> (pure renderSimpleStatus)

{-# NOINLINE this #-}
this :: Identification
this = unsafePerformIO $ fmap (Identification . Text.pack . show) nextRandom

instance {-# OVERLAPPABLE #-} MimeRender HTML (Status a) where
  mimeRender _ st = renderBS $ render st
    where
      render = renderer st

renderSimpleStatus :: Status a -> Html ()
renderSimpleStatus
  (Status (Identification uuid) liveness readiness _ _) =
    html_ $ do
      head_ $ do
        title_ "status page"
        toHtmlRaw @Text "<script async type=\"text/javascript\" src=\"metrics.js\"></script>"
      body_ $ do
        section_ $ do
          h1_ "identification"
          p_ $ toHtml uuid
          renderLiveness liveness
          renderReadiness readiness
        section_ $ do
          h1_ "metrics"
  where
    renderLiveness :: Liveness -> Html ()
    renderLiveness Alive = p_ "alive"

    renderReadiness :: Readiness -> Html ()
    renderReadiness Ready = p_ "ready"
    renderReadiness (Ill reasons) = do
      p_ "not-ready"
      ul_ $ do
        traverse_ renderReason reasons

    renderReason :: Reason -> Html ()
    renderReason (Reason r) =
      li_ $ toHtml r
