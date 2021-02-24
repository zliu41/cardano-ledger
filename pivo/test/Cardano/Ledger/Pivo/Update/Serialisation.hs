{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Pivo.Update.Serialisation where

import qualified Test.Tasty as T
import Test.Tasty.QuickCheck as QC

import Data.Sequence.Strict (StrictSeq(Empty), fromList)
import Data.Text (Text)

import Cardano.Binary (toCBOR)
import Cardano.Crypto.DSIGN (VerKeyDSIGN (VerKeyMockDSIGN), hashVerKeyDSIGN)
import qualified Cardano.Crypto.Hash as Hash

import Shelley.Spec.Ledger.Hashing (HashAnnotated (hashAnnotated))

import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes ()
import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as Mock
import qualified Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip as Roundtrip

import Cardano.Ledger.Pivo (PivoEra)
import Cardano.Ledger.Pivo.Update (Payload (Payload))
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP as SIP

unitTests :: [T.TestTree]
unitTests =
  -- These tests should be replaced by property tests once we add data to the
  -- update payload and state.
  [ QC.testProperty
      "Update payload roundtrip - test 0"
      (Roundtrip.property (Payload @(PivoEra Mock.C_Crypto)
                             Empty -- SIP submissions
                          )
      )
  , QC.testProperty
      "Update payload roundtrip - test 1"
      (Roundtrip.property (Payload @(PivoEra Mock.C_Crypto)
                            (fromList [submission0])
                          )
      )
  ]
 where
   submission0 :: SIP.Submission (PivoEra Mock.C_Crypto)
   submission0 = SIP.Submission vkey0Hash commitHash
   commitHash = Hash.hashWithSerialiser toCBOR (98723, vkey0Hash, proposal0)
   vkey0 = VerKeyMockDSIGN 0912
   vkey0Hash = hashVerKeyDSIGN vkey0
   proposal0 :: SIP.Proposal (PivoEra Mock.C_Crypto)
   proposal0 = SIP.Proposal $ Hash.hashWithSerialiser toCBOR ("Foo" :: Text)
