{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Prod.MimeTypes where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Network.HTTP.Media as M
import Servant
import System.IO.Unsafe

data HTML

data PNG

data SVG

data GraphPictureData
  = GraphPictureData
      { graphvizInput :: !ByteString,
        serializedPng :: (IO ByteString),
        serializedSvg :: (IO ByteString)
      }

instance Accept HTML where
  contentType _ = "text" M.// "html" M./: ("charset", "utf-8")


instance Accept PNG where
  contentType _ = "image" M.// "png"

instance MimeRender PNG GraphPictureData where
  mimeRender _ = fromStrict . unsafePerformIO . serializedPng

instance MimeRender PlainText GraphPictureData where
  mimeRender _ = fromStrict . graphvizInput

instance Accept SVG where
  contentType _ = "image" M.// "svg"

instance MimeRender SVG GraphPictureData where
  mimeRender _ = fromStrict . unsafePerformIO . serializedSvg
