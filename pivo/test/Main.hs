

module Main where

import qualified Test.Tasty as T
import Test.Tasty.HUnit

import qualified Cardano.Ledger.Pivo.Update.Serialisation as Serialisation

main :: IO ()
main = T.defaultMain $ T.testGroup "Serialisation" Serialisation.unitTests
