{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Pivo.Update.Payload.SIP
  (-- * SIP submissions
    Submission.Submission ( Submission.Submission
                          , Submission.author
                          , Submission.commit
                          )
  , Submission.witnesses
  , Submission.mkSubmission
  , Submission.mkCommit
    -- * SIP revelations
  , Revelation.Revelation ( Revelation.Revelation
                          , Revelation.revelator
                          , Revelation.salt
                          )
  , Revelation.mkRevelation
    -- * SIP votes
  , Vote.Vote ( Vote.voter
              , Vote.candidate
              , Vote.confidence
              )
  , Vote.mkVote
  , Vote.voteWitnesses
    -- ** Update module re-exports
  , Proposal.Confidence ( Proposal.For
                        , Proposal.Against
                        , Proposal.Abstain
                        )
  )
where

import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Submission as Submission
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Revelation as Revelation
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP.Vote as Vote

import qualified Cardano.Ledger.Update.Proposal as Proposal
