{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Shelley.Spec.Ledger.Genesis
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson (encode, fromJSON, decode, toJSON)

import qualified Data.ByteString.Lazy.Char8 as LBS

import           Shelley.Spec.Ledger.Address (serialiseAddr, deserialiseAddr)

import           Hedgehog (Property, discover)
import qualified Hedgehog

import           Test.Cardano.Prelude
import           Test.Shelley.Spec.Ledger.Genesis.Example
import           Test.Shelley.Spec.Ledger.Genesis.Gen


prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSON exampleShelleyGenesis "test/Golden/ShelleyGenesis"

prop_roundtrip_GenesisDelegationPair_JSON :: Property
prop_roundtrip_GenesisDelegationPair_JSON =
  -- If this fails, ShelleyGenesis can also fail.
  Hedgehog.property $ do
    dp <- Hedgehog.forAll genGenesisDelegationPair
    Hedgehog.tripping dp toJSON fromJSON
    Hedgehog.tripping dp encode decode

prop_roundtrip_FundPair_JSON :: Property
prop_roundtrip_FundPair_JSON =
  -- If this fails, ShelleyGenesis can also fail.
  Hedgehog.property $ do
    fp <- Hedgehog.forAll genGenesisFundPair
    Hedgehog.tripping fp toJSON fromJSON
    Hedgehog.tripping fp encode decode

prop_roundtrip_ShelleyGenesis_JSON :: Property
prop_roundtrip_ShelleyGenesis_JSON =
  Hedgehog.property $ do
    sg <- Hedgehog.forAll genShelleyGenesis
    Hedgehog.tripping sg toJSON fromJSON
    Hedgehog.tripping sg encode decode

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = do
  -- Set to True when the struct or the JSON encoding changes and you need the new
  -- version.
  if False
    then LBS.writeFile "test/Golden/ShelleyGenesis" (encode exampleShelleyGenesis)
    else pure ()
  Hedgehog.checkParallel $$discover
