{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Pivo.Update.Serialisation where

import qualified Test.Tasty as T
import Test.Tasty.QuickCheck as QC

import Data.Sequence.Strict (StrictSeq(Empty), fromList)

import Cardano.Crypto.DSIGN (VerKeyDSIGN (VerKeyMockDSIGN))

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
                             Empty -- SIP revelations
                             Empty
                          )
      )
  , QC.testProperty
      "Update payload roundtrip - test 1"
      (Roundtrip.property (Payload @(PivoEra Mock.C_Crypto)
                            (fromList [submission0])
                            (fromList [revelation0])
                            (fromList [vote0])
                          )
      )
  , QC.testProperty
      "Update payload roundtrip - test 2"
      (Roundtrip.property (Payload @(PivoEra Mock.C_Crypto)
                            (fromList [ submission0
                                      , submission1
                                      ]
                            )
                            (fromList [ revelation0
                                      , revelation1
                                      ]
                            )
                            (fromList [ vote0
                                      , vote1
                                      , vote2
                                      ])
                          )
      )
  ]
 where
   submission0 :: SIP.Submission (PivoEra Mock.C_Crypto)
   submission0 = SIP.mkSubmission vkey0 salt0 "Foo"
   vkey0 = VerKeyMockDSIGN 0912
   salt0 = 9823
   revelation0 = SIP.mkRevelation vkey0 salt0 "Foo"
   vote0 = SIP.mkVote vkey0 "Foo" SIP.For
   submission1 :: SIP.Submission (PivoEra Mock.C_Crypto)
   submission1 = SIP.mkSubmission vkey1 salt1 "Bar"
   vkey1 = VerKeyMockDSIGN 74551
   salt1 = 2389
   revelation1 = SIP.mkRevelation vkey1 salt1 "Bar"
   vote1 = SIP.mkVote vkey0 "Bar" SIP.Against
   vote2 = SIP.mkVote vkey1 "Bar" SIP.Abstain
