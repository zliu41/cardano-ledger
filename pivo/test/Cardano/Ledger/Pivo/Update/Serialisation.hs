{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Pivo.Update.Serialisation where

import qualified Test.Tasty as T
import Test.Tasty.QuickCheck as QC

import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as Mock
import qualified Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip as Roundtrip

import  Cardano.Ledger.Pivo.Update (Payload (Payload))

unitTests :: [T.TestTree]
unitTests =
  -- These tests should be replaced by property tests once we add data to the
  -- update payload and state.
  [ QC.testProperty "Update payload roundtrip" (Roundtrip.property (Payload @Mock.C_Crypto)) ]
