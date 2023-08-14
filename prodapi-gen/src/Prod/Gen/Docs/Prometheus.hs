module Prod.Gen.Docs.Prometheus where

import Prod.Prometheus
import Servant.Docs
import Data.Proxy
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString

instance ToSample CORSAllowOrigin where
  toSamples _ =
     []

instance ToSample PrometheusResult where
  toSamples _ =
     [ ("some prometheus example", PrometheusResult promdoc) ]
    where
        promdoc :: ByteString
        promdoc = ByteString.unlines
           [ "# HELP prodapi_dns_discovery_results "
           , "# TYPE prodapi_dns_discovery_results gauge"
           , "prodapi_dns_discovery_results{m=\"dig\",type=\"A\",host=\"dicioccio.fr\"} 3.0"
           , "# HELP prodapi_dns_discoveries "
           , "# TYPE prodapi_dns_discoveries counter"
           , "prodapi_dns_discoveries{m=\"dig\",type=\"A\",host=\"dicioccio.fr\"} 257.0"
           , "# HELP prodapi_watchdog_filetouch "
           , "# TYPE prodapi_watchdog_filetouch counter"
           , "prodapi_watchdog_filetouch{status=\"success\",path=\"./example-prodapi-watchdog\"} 516.0"
           , "# HELP userauth_recovery_applied number of recovery-requests applied"
           , "# TYPE userauth_recovery_applied counter"
           , "userauth_recovery_applied{status=\"ok\"} 1.0"
           , "userauth_recovery_applied{status=\"requested\"} 2.0"
           , "# HELP userauth_recovery_requests number of recovery-requests received"
           , "# TYPE userauth_recovery_requests counter"
           , "userauth_recovery_requests{status=\"ok\"} 1.0"
           , "userauth_recovery_requests{status=\"requested\"} 1.0"
           , "# HELP userauth_registrations number of registrations"
           , "# TYPE userauth_registrations counter"
           , "userauth_registrations{status=\"ok\"} 1.0"
           , "userauth_registrations{status=\"requested\"} 2.0"
           , "# HELP watchdog_hello continuously working"
           , "# TYPE watchdog_hello counter"
           , "watchdog_hello{status=\"success\"} 5147.0"
           ]


run :: IO ()
run = putStrLn $ markdown $ docs (Proxy @PrometheusApi)
