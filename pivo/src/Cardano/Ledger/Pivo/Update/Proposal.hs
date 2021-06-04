-- | Re-exports of Priviledge proposal types for convenience.
module Cardano.Ledger.Pivo.Update.Proposal
  ( Proposal.Confidence ( Proposal.For
                        , Proposal.Against
                        , Proposal.Abstain
                        )
  , Proposal._id
  )
where

import qualified Cardano.Ledger.Update.Proposal as Proposal
