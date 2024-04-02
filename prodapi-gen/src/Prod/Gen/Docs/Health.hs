module Prod.Gen.Docs.Health where

import Data.Proxy
import qualified Data.Set as Set
import Prod.Health
import Servant.Docs

instance ToSample Liveness where
    toSamples _ =
        [("an application is alive if it can returns some string, hence there is a single value possible", Alive)]

instance ToSample Readiness where
    toSamples _ =
        [ ("ready to serve requests", Ready)
        , ("should not be serving requests for _some reason_", Ill (Set.fromList [Reason "some reason"]))
        ]

run :: IO ()
run = putStrLn $ markdown $ docs (Proxy @HealthApi)
