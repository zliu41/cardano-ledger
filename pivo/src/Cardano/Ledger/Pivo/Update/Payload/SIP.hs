{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
  , wrapSIPSubmission
  , wrapSIPRevelation
  , wrapSIPVote
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
  , SIP.Proposal
  , SIP.mkProposal
    -- * Id's
  , Id (VoterId, SIP.ProposalId)
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

import Data.Typeable (Typeable)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

import Cardano.Binary (ToCBOR, FromCBOR)

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
  , Id
  )

import qualified Cardano.Ledger.Update.Proposal as Proposal
import qualified Cardano.Ledger.Update as Update

import Cardano.Ledger.Era (Era)

import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Submission as SIPS
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Revelation as SIPR
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Vote as SIPV
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal as SIP

instance Era era => Proposal (SIP.Proposal era) where
  newtype Submission (SIP.Proposal era) =
    SIPSubmission { unSIPSubmission :: SIPS.Submission era }
    deriving stock (Eq, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON)

  newtype Revelation (SIP.Proposal era) =
    SIPRevelation { unSIPRevelation :: SIPR.Revelation era }
    deriving stock (Eq, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON)

  newtype Vote       (SIP.Proposal era) =
    SIPVote       { unSIPVote :: SIPV.Vote era }
    deriving stock (Eq, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON)

  newtype Voter      (SIP.Proposal era) =
    SIPVoter      { unSIPVoter :: SIPV.VoterId era }
    deriving stock (Eq, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON)

  revelationCommit = SIPS.commit . unSIPSubmission

  proposal = SIPR.proposal . unSIPRevelation

  votingPeriodDuration = SIP.votingPeriodDuration

  voter = VoterId . SIPV.voter . unSIPVote

  candidate = SIPV.candidate . unSIPVote

  confidence = SIPV.confidence . unSIPVote

wrapSIPSubmission
  :: SIPS.Submission era -> Update.Payload (SIP.Proposal era) impl
wrapSIPSubmission = Update.Ideation . Proposal.Submit . SIPSubmission

wrapSIPRevelation
  :: SIPR.Revelation era -> Update.Payload (SIP.Proposal era) impl
wrapSIPRevelation = Update.Ideation . Proposal.Reveal . SIPRevelation

wrapSIPVote
  :: SIPV.Vote era -> Update.Payload (SIP.Proposal era) impl
wrapSIPVote = Update.Ideation . Proposal.Cast . SIPVote

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
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON, ToJSONKey)
  _id = VoterId . unSIPVoter

deriving newtype instance Era era => FromJSONKey (Id (Voter (SIP.Proposal era)))

deriving newtype instance Era era => FromJSON (Id (Voter (SIP.Proposal era)))

deriving newtype instance
  (Typeable era, Era era) => ToCBOR (Id (Voter (SIP.Proposal era)))

deriving newtype instance
  (Typeable era, Era era) => FromCBOR (Id (Voter (SIP.Proposal era)))
