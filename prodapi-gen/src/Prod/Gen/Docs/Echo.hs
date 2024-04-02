module Prod.Gen.Docs.Echo where

import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Prod.Echo
import Servant.Docs

data Example = Example {helloWorld :: Text}
    deriving (Generic)

instance FromJSON Example
instance ToJSON Example
instance ToSample Example where
    toSamples _ =
        [("hello world, not that the Echo API is parametrizable, this doc line is an instantiated example", Example "hello prodapi")]

run :: IO ()
run = putStrLn $ markdown $ docs (Proxy @(EchoApi "example" Example))
