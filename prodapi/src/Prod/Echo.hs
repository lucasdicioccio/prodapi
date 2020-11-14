{-# LANGUAGE KindSignatures #-}

module Prod.Echo
  ( EchoApi,
    handleEcho,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.TypeLits (Symbol)
import Servant ((:>), JSON, Post, ReqBody, Summary)
import Servant.Server (Handler)

type EchoApi (segment :: Symbol) a =
  Summary "returns the input"
    :> "echo"
    :> segment
    :> ReqBody '[JSON] a
    :> Post '[JSON] a

handleEcho :: (FromJSON a, ToJSON a) => a -> Handler a
handleEcho = pure
