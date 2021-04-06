{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Shelley.Spec.Ledger.UpdateState
  ( HasUpdateState (..)
  ) where

import Cardano.Ledger.Era

import Shelley.Spec.Ledger.Slot (EpochNo)

import Data.Kind (Type)

class Era era => HasUpdateState era where
  type ProposalUpdate era :: Type
  type VoteUpdate era :: Type

  type GovernanceProposal era :: Type -- This could include parameter deltas or more far-reaching changes

  type IdeationState era :: Type
  type ApprovalState era :: Type

  initialIdeationState :: IdeationState era
  initialApprovalState :: ApprovalState era

  submitProposal :: ProposalUpdate era -> IdeationState era -> IdeationState era
  submitVote :: VoteUpdate era -> ApprovalState era -> ApprovalState era

  voteableProposals :: ApprovalState era -> [GovernanceProposal era] -- TODO: Do we need this?

  -- This can clear out old proposals/votes, move things from ideation to voting,
  -- and also returns changes that have met with final approval
  processEpoch
    :: EpochNo
    -> IdeationState era
    -> ApprovalState era
    -> (IdeationState era, ApprovalState era, [GovernanceProposal era])
