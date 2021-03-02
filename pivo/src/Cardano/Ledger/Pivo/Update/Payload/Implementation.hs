{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of an SIP
module Cardano.Ledger.Pivo.Update.Payload.Implementation where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Aeson (ToJSON, FromJSON)

import qualified Cardano.Crypto.Hash as Cardano
import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf)

import           Cardano.Slotting.Slot (SlotNo)

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
  , Confidence (Abstain, Against, For)
  )

import qualified Cardano.Ledger.Update.Proposal as Proposal

import Cardano.Ledger.Era (Era)

import qualified Cardano.Ledger.Era as Era


import Shelley.Spec.Ledger.Credential (Credential (KeyHashObj))

import qualified Shelley.Spec.Ledger.Keys as Shelley

import Cardano.Ledger.Pivo.Update.Payload.Types (Hash, VKeyHash, VKey)

import qualified Cardano.Ledger.Pivo.Update.Payload.SIP as SIP

data Implementation era =
  Implementation
    { sipId                    :: Hash era (SIP.Proposal era)
    , implVotingPeriodDuration :: SlotNo
    }
  deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

instance Era era => FromJSON (Implementation era)

instance Era era => Identifiable (Implementation era) where
  newtype Id (Implementation era) =
    ImplementationId { unImplementationId :: Hash era (Implementation era)}
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON)

  _id = ImplementationId . Cardano.hashWithSerialiser toCBOR

deriving instance Era era => FromJSON (Id (Implementation era))

instance (Typeable era, Era era) => ToCBOR (Implementation era) where
  toCBOR Implementation { sipId } =
    encodeListLen 1 <> toCBOR sipId

instance Era era => Proposal (Implementation era) where
  data Submission (Implementation era) =
    ImplSubmission
      { submissionAuthor :: VKeyHash era
      , submissionCommit :: Commit (Revelation (Implementation era))
      }
    deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

  data Revelation (Implementation era) =
    ImplRevelation
      { revealedImplementation :: Implementation era
      , revelator              :: VKeyHash era
      , revelationSalt         :: Int
      }
    deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

  data Vote (Implementation era) =
    ImplVote
      { implVoter :: Voter (Implementation era)
      , implCandidate :: Id (Implementation era)
      , implConfidence :: Confidence
      }
    deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

  newtype Voter (Implementation era) =
    ImplVoter { unImplVoter :: Credential 'Shelley.Staking (Era.Crypto era) }
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON)

  revelationCommit = submissionCommit -- TODO: There's something wrong with the names here :/

  proposal = revealedImplementation

  votingPeriodDuration = implVotingPeriodDuration

  voter = _id . implVoter

  candidate = implCandidate

  confidence = implConfidence

instance Era era => Commitable (Revelation (Implementation era)) where
  type Commit (Revelation (Implementation era)) =
    Hash era (Int, VKeyHash era, Hash era (Implementation era))

  commit r = Cardano.hashWithSerialiser toCBOR
           $ ( revelationSalt r
             , revelator r
             , unImplementationId $ _id $ revealedImplementation r
             )

instance Signed (Submission (Implementation era)) where
  signatureVerifies = const True

instance Signed (Vote (Implementation era)) where
  signatureVerifies = const True

instance Identifiable (Voter (Implementation era)) where
  newtype Id (Voter (Implementation era)) =
    VoterId { unVoterId :: Voter (Implementation era) }
    deriving (Eq, Ord, Show)

  _id = VoterId
