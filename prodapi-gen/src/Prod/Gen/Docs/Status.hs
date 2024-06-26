module Prod.Gen.Docs.Status where

import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Lucid
import Prod.Health
import Prod.Status
import Servant.Docs

data Example = Example {exampleStatus :: Text}
    deriving (Generic)
instance ToJSON Example

instance ToHtml Example where
    toHtml (Example txt) = section_ $ do
        h1_ $ toHtml txt
        p_ "note that you can tune your status page"
    toHtmlRaw (Example txt) = section_ $ toHtml txt

instance ToSample Example where
    toSamples _ =
        [ ("an example status, you can customize the status content in your own applications.", Example "example")
        ]

instance ToSample (Status Example) where
    toSamples _ =
        [
            ( "a status page recapitulates liveness, healthiness, and has extras"
            , Status this Alive Ready (Example "example tunable status") statusPage
            )
        ]

run :: IO ()
run = putStrLn $ markdown $ docs (Proxy @(StatusApi Example))
