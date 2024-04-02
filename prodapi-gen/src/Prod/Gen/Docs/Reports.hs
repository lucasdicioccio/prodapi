module Prod.Gen.Docs.Reports where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Prod.Reports
import Servant.Docs

data Example = Example {stackTrace :: [Text]}
    deriving (Generic)
instance ToJSON Example
instance FromJSON Example

instance ToSample Int where
    toSamples _ =
        [("an example integer", 42)]
instance ToSample (Report Example) where
    toSamples _ =
        [("an example of stack-trace reporting", Report 1611183428 0 [Example ["err toto.js at 236: undefined is not a function"]])]

run :: IO ()
run = putStrLn $ markdown $ docs (Proxy @(ReportsApi Example))
