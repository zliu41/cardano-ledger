{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Pivo.Update.Serialisation where

import qualified Test.Tasty as T
import Test.Tasty.QuickCheck as QC

import Data.Sequence.Strict (StrictSeq(Empty), fromList)

import Cardano.Crypto.DSIGN (VerKeyDSIGN (VerKeyMockDSIGN))

import Cardano.Ledger.Update.Proposal (Confidence (For), _id)

import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes ()
import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as Mock
import qualified Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip as Roundtrip

import Cardano.Ledger.Pivo (PivoEra)
import Cardano.Ledger.Pivo.Update (Payload (Payload))
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP as SIP
import qualified Cardano.Ledger.Pivo.Update.Payload.Implementation as IMP

unitTests :: [T.TestTree]
unitTests =
  -- These tests should be replaced by property tests once we add data to the
  -- update payload and state.
  [ QC.testProperty
      "Update payload roundtrip - test 0"
      (Roundtrip.property (Payload @(PivoEra Mock.C_Crypto)
                             Empty -- SIP submissions
                             Empty -- SIP revelations
                             Empty -- SIP Votes
                             Empty -- Implementation submissions
                             Empty -- Implementation revelations
                             Empty -- Implementation votes
                             Empty -- Endorsements
                          )
      )
  , QC.testProperty
      "Update payload roundtrip - test 1"
      (Roundtrip.property (Payload @(PivoEra Mock.C_Crypto)
                            (fromList [submission0])
                            (fromList [revelation0])
                            (fromList [vote0])
                            (fromList [impSubmission0])
                            (fromList [impRevelation0])
                            (fromList [impVote0])
                            (fromList [endorsement0])
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
                                      ]
                            )
                            (fromList [ impSubmission0
                                      , impSubmission1
                                      ]
                            )
                            (fromList [ impRevelation0
                                      , impRevelation1
                                      ]
                            )
                            (fromList [ impVote0
                                      , impVote1
                                      ]
                            )
                            (fromList [ endorsement0
                                      , endorsement1
                                      ]
                            )
                          )
      )
  ]
 where
   -- Ideation
   submission0 :: SIP.Submission (PivoEra Mock.C_Crypto)
   submission0 = SIP.mkSubmission vkey0 salt0 proposal0
   proposal0 = SIP.mkProposal "Foo" 100
   vkey0 = VerKeyMockDSIGN 0912
   salt0 = 9823
   revelation0 = SIP.mkRevelation vkey0 salt0 proposal0
   vote0 = SIP.mkVote vkey0 (_id proposal0) For
   submission1 :: SIP.Submission (PivoEra Mock.C_Crypto)
   submission1 = SIP.mkSubmission vkey1 salt1 proposal1
   proposal1 = SIP.mkProposal "Bar" 300
   vkey1 = VerKeyMockDSIGN 74551
   salt1 = 2389
   revelation1 = SIP.mkRevelation vkey1 salt1 proposal1
   vote1 = SIP.mkVote vkey0 (_id proposal1) SIP.Against
   vote2 = SIP.mkVote vkey1 (_id proposal1) SIP.Abstain
   -- Approval
   impSubmission0 = IMP.mkSubmission vkey0 salt0 implementation0
   implementation0 = IMP.mkImplementation (SIP.unProposalId $ _id proposal0) 100 protocol1
   protocol1 = IMP.mkProtocol 1 IMP.protocolZero
   protocol2 = IMP.mkProtocol 2 IMP.protocolZero
   impSubmission1 = IMP.mkSubmission vkey1 salt1 implementation1
   implementation1 = IMP.mkImplementation (SIP.unProposalId $ _id proposal1) 200 protocol2
   impRevelation0 = IMP.mkRevelation vkey0 salt0 implementation0
   impRevelation1 = IMP.mkRevelation vkey1 salt1 implementation1
   impVote0 = IMP.mkVote vkey0 (_id implementation0) For
   impVote1 = IMP.mkVote vkey1 (_id implementation1) For
   -- Activation
   endorsement0 = IMP.mkEndorsement vkey0 1
   endorsement1 = IMP.mkEndorsement vkey1 2
