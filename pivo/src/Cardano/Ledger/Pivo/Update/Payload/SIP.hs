{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Pivo.Update.Payload.SIP
  (-- * SIP submissions
    SIPS.Submission ( SIPS.Submission
                   , SIPS.author
                   , SIPS.commit
                   )
  , SIPS.witnesses
  , SIPS.mkSubmission
  , SIPS.mkCommit
    -- * SIP revelations
  , SIPR.Revelation ( SIPR.Revelation
                   , SIPR.revelator
                   , SIPR.salt
                   )
  , SIPR.mkRevelation
    -- * SIP votes
  , SIPV.Vote ( SIPV.voter
              , SIPV.candidate
              , SIPV.confidence
              )
  , SIPV.mkVote
  , SIPV.voteWitnesses
    -- * Proposals
  , SIP.mkProposal
    -- ** Proposal newtype functions
  , unSIPSubmission
  , unSIPRevelation
  , unSIPVote
  , unSIPVoter, Voter(SIPVoter)
    -- ** Update module re-exports
  , Proposal.Confidence ( Proposal.For
                        , Proposal.Against
                        , Proposal.Abstain
                        )
  , Proposal._id
  )
where

import Cardano.Ledger.Update.Proposal
  ( Commitable
  , Commit
  , Proposal ( Revelation
             , Submission
             , Vote
             , Voter
             , revelationCommit
             , proposal
             , votingPeriodDuration
             , voter
             , candidate
             , confidence
             )
  , Identifiable (_id)
  , Signed (signatureVerifies)
  )
import qualified Cardano.Ledger.Update.Proposal as Proposal

import Cardano.Ledger.Era (Era)

import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Submission as SIPS
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Revelation as SIPR
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Vote as SIPV
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal as SIP

instance Era era => Proposal (SIP.Proposal era) where
  newtype Submission (SIP.Proposal era) =
    SIPSubmission { unSIPSubmission :: SIPS.Submission era }
    deriving (Show)
  newtype Revelation (SIP.Proposal era) =
    SIPRevelation { unSIPRevelation :: SIPR.Revelation era }
    deriving (Show)
  newtype Vote       (SIP.Proposal era) =
    SIPVote       { unSIPVote :: SIPV.Vote era }
    deriving (Show)
  newtype Voter      (SIP.Proposal era) =
    SIPVoter      { unSIPVoter :: SIPV.VoterId era}

  revelationCommit = SIPS.commit . unSIPSubmission

  proposal = SIPR.proposal . unSIPRevelation

  votingPeriodDuration = SIP.votingPeriodDuration

  voter = VoterId . SIPV.voter . unSIPVote

  candidate = SIPV.candidate . unSIPVote

  confidence = SIPV.confidence . unSIPVote

instance Era era => Commitable (Revelation (SIP.Proposal era)) where
  type Commit (Revelation (SIP.Proposal era)) = SIPS.Commit era

  commit (SIPRevelation r) =
    SIPS.mkCommit (SIPR.salt r) (SIPR.revelator r) (SIPR.proposal r)

instance Signed (Submission (SIP.Proposal era)) where
  -- See comment in the Signed instance for SIP.Proposal
  signatureVerifies = const True

instance Signed (Vote (SIP.Proposal era)) where
  -- See comment in the Signed instance for SIP.Proposal
  signatureVerifies = const True

instance Identifiable (Voter (SIP.Proposal era)) where
  newtype Id (Voter (SIP.Proposal era)) = VoterId (SIPV.VoterId era)
    deriving (Eq, Ord, Show)
  _id = VoterId . unSIPVoter
