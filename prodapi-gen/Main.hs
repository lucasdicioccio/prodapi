module Main where

import Control.Monad (void)
import qualified Prod.Gen.Docs.Echo
import qualified Prod.Gen.Docs.Health
import qualified Prod.Gen.Docs.Prometheus
import qualified Prod.Gen.Docs.Reports
import qualified Prod.Gen.Docs.Status
import qualified Prod.Gen.Docs.UserAuth
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (k : v : _) -> case lookup (k, v) registry of
            Just io -> io
            Nothing -> notFound k v
        _ -> help

notFound :: String -> String -> IO ()
notFound k v = do
    print $ "generator for type " ++ v ++ " not found for key " ++ k
    exitFailure

help :: IO ()
help = void $ do
    putStrLn "usage: prodapi-gen <key> <type>"
    putStrLn "known <key> <type>:"
    let printKeyType (k, t) = putStrLn $ "- " ++ k ++ " " ++ t
    traverse printKeyType $ fmap fst registry

registry :: [((String, String), IO ())]
registry =
    [ (("docs", "health"), Prod.Gen.Docs.Health.run)
    , (("docs", "echo"), Prod.Gen.Docs.Echo.run)
    , (("docs", "status"), Prod.Gen.Docs.Status.run)
    , (("docs", "user-auth"), Prod.Gen.Docs.UserAuth.run)
    , (("docs", "reports"), Prod.Gen.Docs.Reports.run)
    , (("docs", "prometheus"), Prod.Gen.Docs.Prometheus.run)
    ]
