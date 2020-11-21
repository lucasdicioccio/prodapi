module Main where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Monitor (component)

foreign import tabUrl :: (String -> Effect Unit) -> Effect Unit

main :: Effect Unit
main = do
  ref <- Ref.new ""
  tabUrl (\url -> Ref.write url ref)
  runHalogenAff do
    body <- awaitBody
    runUI (component ref) unit body
