{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Ledger.Pivo.Update.Payload.SIP2 where

import           Control.DeepSeq                                 (NFData)
import           Data.Aeson                                      (FromJSON,
                                                                  ToJSON)
import           Data.Set                                        (Set,
                                                                  singleton)
import           Data.Typeable                                   (Typeable)
import           GHC.Generics                                    (Generic)
import           NoThunks.Class                                  (NoThunks)

import           Cardano.Binary                                  (FromCBOR (fromCBOR),
                                                                  ToCBOR (toCBOR),
                                                                  decodeListLenOf,
                                                                  encodeListLen)
import           Cardano.Crypto.DSIGN                            (hashVerKeyDSIGN)
import qualified Cardano.Crypto.Hash                             as Cardano

import           Cardano.Ledger.Era                              (Era)
import qualified Cardano.Ledger.Era                              as Era
import           Shelley.Spec.Ledger.Credential                  (Credential (KeyHashObj))

import qualified Shelley.Spec.Ledger.Keys                        as Shelley

import           Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal (Proposal)
import           Cardano.Ledger.Pivo.Update.Payload.Types        (VKey)
import           Cardano.Ledger.Update.Proposal                  (Confidence,
                                                                  Id, _id)
import qualified Cardano.Ledger.Update.Proposal                  as Proposal

import qualified Shelley.Spec.Ledger.Keys                        as Shelley

import           Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal (Proposal,
                                                                  unProposalId)
import           Cardano.Ledger.Pivo.Update.Payload.Types        (Hash, VKey,
                                                                  VKeyHash)


data Proposal era =
  Proposal
    { proposalTextHash     :: Hash era Text
      -- ^ Hash of the proposal's text. For now we assume the proposal is simply
      -- a string.
    , votingPeriodDuration :: SlotNo
    }
  deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

deriving instance Era era => FromJSON (Proposal era)

mkProposal :: Era era => Text -> SlotNo -> Proposal era
mkProposal someText duration =
  Proposal
    { proposalTextHash     = Cardano.hashWithSerialiser toCBOR someText
    , votingPeriodDuration = duration
    }

--------------------------------------------------------------------------------
-- Proposal instance
--------------------------------------------------------------------------------

instance Era era => Proposal (Proposal era) where
  data Submission (Proposal era) =
    SIPSubmission
      { author :: VKeyHash era
        -- ^ Submission author. This will be compared against the revelator key
        -- in 'Revelation'.
      , commit :: Commit era
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON)

  data Revelation (Proposal era) =
    SIPRevelation
      { proposal  :: Proposal era
       -- ^ Proposal that is being revealed.
      , revelator :: VKeyHash era
        -- ^ Revelation author. This should coincide with the submission author.
      , salt      :: Int
        -- ^ Salt used to calculate the commit.
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON)

  data Vote (Proposal era) =
    SIPVote
      { voter      :: VoterId era
      , candidate  :: Id (Proposal era)
      , confidence :: Confidence
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON, ToJSONKey, FromJSON)

  newtype Voter (Proposal era) =
     SIPVoter { unSIPVoter :: Credential 'Shelley.Staking (Era.Crypto era) }
     deriving stock (Eq, Ord, Show, Generic)
     deriving newtype (NFData, NoThunks, ToJSON, ToJSONKey)

  revelationCommit = commit

  proposal = proposal

  votingPeriodDuration = votingPeriodDuration

  candidate = candidate

  confidence = confidence

deriving instance Era era => FromJSON (Submission (Proposal era))
deriving newtype instance Era era => FromJSON (Voter (Proposal era))
deriving newtype instance Era era => FromJSONKey (Voter (Proposal era))

mkSubmission
  :: Era era
  => VKey era
  -- ^ Proposal's author.
  -> Int
  -- ^ Salt used to calculate the submission commit.
  -> Proposal era
  -> Submission (Proposal era)
mkSubmission vk someSalt someProposal =
  Submission
    { author = vkHash
    , commit = Proposal.commit (mkRevelation someSalt vkHash someProposal)
    }

mkRevelation
  :: Era era
  => VKey era
  -> Int
  -> Proposal era
  -> Revelation (Proposal era)
mkRevelation vk someSalt someProposal =
  Revelation
    { proposal  = someProposal
    , revelator = hashVerKeyDSIGN vk
    , salt      = someSalt
    }

mkVote
  :: forall era
   . Era era
  => VKey era
  -> Id (Proposal era)
  -> Confidence
  -> Vote (Proposal era)
mkVote vk proposalId someConfidence =
  Vote
    { voter      = KeyHashObj $ Shelley.KeyHash $ hashVerKeyDSIGN vk
    , candidate  = proposalId
    , confidence = someConfidence
    }

--------------------------------------------------------------------------------
-- Commitable instance
--------------------------------------------------------------------------------

instance Era era => Commitable (Revelation (Proposal era)) where
  type Commit (Revelation (Proposal era)) =
    ( VKeyHash era -- Commit author
    , Hash era (Int, VKeyHash era, Hash era (Proposal era))
    )

  commit r =
    ( revelator r
    , Cardano.hashWithSerialiser toCBOR
      $ ( salt r
        , revelator r
        , unImplementationId $ _id $ proposal r
        )
    )

--------------------------------------------------------------------------------
-- Identifiable instances
--------------------------------------------------------------------------------

instance Era era => Identifiable (Proposal era) where
  newtype Id (Proposal era) =
    ProposalId { unProposalId :: Hash era (Proposal era) }
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey)

  _id = ProposalId . Cardano.hashWithSerialiser toCBOR

deriving newtype instance Era era => FromJSON (Id (Proposal era))
deriving newtype instance Era era => FromJSONKey (Id (Proposal era))

instance Identifiable (Voter (Proposal era)) where
  newtype Id (Voter (Proposal era)) =
    VoterId { unVoterId :: Voter (Proposal era)}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

  _id = VoterId

--------------------------------------------------------------------------------
-- Signed instances
--------------------------------------------------------------------------------

instance Signed (Proposal era) where
  -- This is an mismatch between Shelley and the update mechanism design: the
  -- update mechanism requires that we can verify the proposal's signature,
  -- however in the Shelley design, signing the payload is outside the scope of
  -- other sub-systems (like the update sub-system) due to the use of the
  -- segregated witness approach.
  --
  -- Given that using segregated witnesses make sense in the context of
  -- blockchain systems it is the update sub-module that has to be adapted.
  signatureVerifies = const True

instance Signed (Submission (Proposal era)) where
  -- See comment in the Signed instance for @Proposal era@.
  signatureVerifies = const True

instance Signed (Vote (Proposal era)) where
  -- See comment in the Signed instance for @Proposal era@.
  signatureVerifies = const True

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance (Typeable era, Era era) => ToCBOR (Proposal era) where
  toCBOR (Proposal { proposalTextHash, votingPeriodDuration }) =
    encodeListLen 2 <> toCBOR proposalTextHash <> toCBOR votingPeriodDuration

instance (Typeable era, Era era) => FromCBOR (Proposal era) where
  fromCBOR = do
    decodeListLenOf 2
    h <- fromCBOR
    d <- fromCBOR
    return $! Proposal h d

-- TODO: make these instances equal to their counterparts
instance (Typeable era, Era era) => ToCBOR (Id (Proposal era)) where
  toCBOR (ProposalId hash) = toCBOR hash

instance (Typeable era, Era era) => FromCBOR (Id (Proposal era)) where
  fromCBOR = ProposalId <$> fromCBOR

deriving instance Era era => FromJSON (Proposal era)

--------------------------------------------------------------------------------
-- Payload wrapping functions
--------------------------------------------------------------------------------

wrapSIPSubmission
  :: SIPS.Submission era -> Update.Payload (SIP.Proposal era) impl
wrapSIPSubmission = Update.Ideation . Proposal.Submit . SIPSubmission

wrapSIPRevelation
  :: SIPR.Revelation era -> Update.Payload (SIP.Proposal era) impl
wrapSIPRevelation = Update.Ideation . Proposal.Reveal . SIPRevelation

wrapSIPVote
  :: SIPV.Vote era -> Update.Payload (SIP.Proposal era) impl
wrapSIPVote = Update.Ideation . Proposal.Cast . SIPVote
