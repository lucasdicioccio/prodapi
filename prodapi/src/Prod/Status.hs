{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prod.Status
  ( StatusApi,
    RenderStatus,
    defaultStatusPage,
    metricsSection,
    versionsSection,
    statusPage,
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
import Data.Version (Version, showVersion)

import Lucid

type StatusApi a =
  "status"
    :> Get '[HTML,JSON] (Status a)

newtype Identification = Identification Text
  deriving
    (ToJSON)
    via Text

-- | Type to render a status page.
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

handleStatus :: Runtime -> IO a -> RenderStatus a -> Handler (Status a)
handleStatus runtime getAppStatus render =
  liftIO $
    Status this
      <$> Health.liveness runtime
      <*> Health.completeReadiness runtime
      <*> getAppStatus
      <*> (pure render)

{-# NOINLINE this #-}
this :: Identification
this = unsafePerformIO $ fmap (Identification . Text.pack . show) nextRandom

instance {-# OVERLAPPABLE #-} MimeRender HTML (Status a) where
  mimeRender _ st = renderBS $ render st
    where
      render = renderer st

defaultStatusPage :: forall a. (a -> Html ()) -> RenderStatus a
defaultStatusPage renderAppStatus = go
  where
    go :: Status a -> Html ()
    go (Status (Identification uuid) liveness readiness appStatus _) =
      html_ $ do
        head_ $ do
          title_ "status page"
          link_ [ rel_ "stylesheet", type_ "text/css", href_ "status.css"]
          toHtmlRaw @Text "<script async type=\"text/javascript\" src=\"metrics.js\"></script>"
        body_ $ do
          section_ $ do
            h1_ "identification"
            p_ $ toHtml uuid
          section_ $ do
            h1_ "general status"
            renderLiveness liveness
            renderReadiness readiness
            with form_ [ action_ "/health/drain" , method_ "post" ] $
              input_ [ type_ "submit", value_ "drain me" ]
          section_ $ do
            h1_ "app status"
            renderAppStatus appStatus
    renderLiveness :: Liveness -> Html ()
    renderLiveness Alive = p_ $ with a_ [ href_ "/health/alive" ] "alive"

    renderReadiness :: Readiness -> Html ()
    renderReadiness Ready = p_ $ with a_ [ href_ "/health/ready" ] "ready"
    renderReadiness (Ill reasons) = do
      p_ $ with a_ [ href_ "/health/ready" ] "not-ready"
      ul_ $ do
        traverse_ renderReason reasons

    renderReason :: Reason -> Html ()
    renderReason (Reason r) =
      li_ $ toHtml r

-- | Like defaultStatusPage but uses a type-class-defined to pass the
-- application-status rendering.
statusPage :: ToHtml a => RenderStatus a
statusPage = defaultStatusPage toHtml

-- | Section with metrics.
metricsSection :: RenderStatus a
metricsSection = const $
  section_ $ do
    h1_ "metrics"
    with div_ [ id_ "metrics" ] $ pure ()

versionsSection :: [(String, Version)] -> RenderStatus a
versionsSection pkgs = const $
  section_ $ do
    h1_ "versions"
    ul_ $
      traverse_ renderVersion pkgs
  where
    renderVersion :: (String, Version) -> Html ()
    renderVersion (pkg, ver) =
       li_ $ p_ $ toHtml $ pkg <> ":" <> showVersion ver
